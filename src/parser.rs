mod parse;

use std::{
    collections::hash_map::{Entry, HashMap},
    mem,
    rc::Rc,
    str,
};

use crate::{
    error::{ErrorKind, Errors, Token},
    DecodeTree, Insn, Str, UnnamedField,
};

pub use parse::Span;

type FieldItem<'a, S = Span<'a>> = super::FieldItem<S>;
type FieldDef<'a, S = Span<'a>> = super::FieldDef<S>;
type FieldRef<'a, S = Span<'a>> = super::FieldRef<S>;
type Field<'a, S = Span<'a>> = super::Field<S>;
type ArgsValueKind<'a, S = Span<'a>> = super::SetValueKind<S>;
type ArgsValue<'a, S = Span<'a>> = super::SetValue<S>;
type ValueKind<'a, S = Span<'a>> = super::ValueKind<S>;
type Value<'a, S = Span<'a>> = super::Value<S>;
type Cond<'a, S = Span<'a>> = super::Cond<S>;
type Pattern<'a, I, S = Span<'a>> = super::Pattern<I, S>;
type OverlapItem<'a, I, S = Span<'a>> = super::OverlapItem<I, S>;
type Overlap<'a, I, S = Span<'a>> = super::Overlap<I, S>;
type Item<'a, I, S = Span<'a>> = super::Item<I, S>;
type Group<'a, I, S = Span<'a>> = super::Group<I, S>;

#[derive(Clone, Debug)]
struct ArgsDef<'src> {
    name: Span<'src>,
    is_extern: bool,
    items: Vec<ArgsValue<'src>>,
}

impl<'src, S> From<ArgsDef<'src>> for super::SetDef<S>
where
    S: From<&'src str>,
{
    fn from(other: ArgsDef<'src>) -> super::SetDef<S> {
        Self {
            name: S::from(&other.name),
            is_extern: other.is_extern,
            items: other
                .items
                .iter()
                .map(|i| super::SetValueDef {
                    name: S::from(&i.name),
                    ty: i.ty.map(|s| S::from(&s)),
                })
                .collect(),
        }
    }
}

/// Decodetree parser.
pub struct Parser<'src, I = super::DefaultInsn, S = Str> {
    src: &'src str,
    insn_size: Rc<[u32]>,
    insn_size_max: u32,
    is_fixed_insn: bool,
    fields: HashMap<&'src str, Rc<FieldDef<'src>>>,
    fields_tree: HashMap<Str, Rc<super::FieldDef<S>>>,
    args: HashMap<&'src str, ArgsDef<'src>>,
    formats: HashMap<&'src str, Pattern<'src, I>>,
    root: parse::Group<'src>,
    errors: Errors<'src>,
}

impl<'src, I, S> Parser<'src, I, S>
where
    I: Insn,
    S: Ord + From<&'src str>,
{
    /// Creates a new parser for decodetree input in `src`.
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            insn_size: Rc::from([I::width()]),
            insn_size_max: I::width(),
            is_fixed_insn: true,
            errors: Errors::new(src),
            fields: Default::default(),
            fields_tree: Default::default(),
            args: Default::default(),
            formats: Default::default(),
            root: Default::default(),
        }
    }

    /// Set valid pattern sizes.
    ///
    /// # Panics
    ///
    /// If size is empty or max size is greater than `I::width()`.
    pub fn set_insn_size(mut self, size: &[u32]) -> Self {
        assert!(!size.is_empty());
        let mut vec = Vec::from(size);
        vec.sort();
        vec.dedup();
        self.insn_size = Rc::from(vec.into_boxed_slice());
        self.insn_size_max = self.insn_size.last().copied().unwrap();
        assert!(self.insn_size_max <= I::width());
        self.is_fixed_insn = self.insn_size.len() == 1;
        self
    }

    fn check_field_range(&mut self, pos: parse::Number<'src, u32>, len: parse::Number<'src, u32>) {
        if len.value == 0 {
            self.errors.push(len.span, ErrorKind::FieldLenZero);
        }
        if pos.value >= self.insn_size_max {
            self.errors
                .field_pos(pos.span, self.insn_size_max, pos.value);
        } else if pos.value + len.value > self.insn_size_max {
            self.errors
                .field_pos(len.span, self.insn_size_max, pos.value + len.value);
        }
    }

    fn add_field_def(&mut self, def: parse::FieldDef<'src>) {
        use parse::Field as E;

        let mut items = Vec::with_capacity(def.items.len());

        for i in &def.items {
            match i {
                E::Unnamed(field) => {
                    self.check_field_range(field.pos, field.len);
                    let item = FieldItem::field(field.pos(), field.len(), field.sign_extend());
                    items.push(item);
                }
                E::Named(r) => {
                    if let Some(field) = self.fields.get(r.name.fragment()) {
                        let item = FieldItem::field_ref(field.clone(), r.len(), r.sign_extend());
                        items.push(item);
                    } else {
                        self.errors.undefined(r.name, Token::Field);
                    }
                }
            };
        }

        let field = FieldDef {
            name: def.name,
            func: def.func,
            items,
        };
        let field_def = Rc::new(self.convert_field_def(&field));

        match self.fields.entry(def.name.fragment()) {
            Entry::Vacant(e) => {
                e.insert(Rc::new(field));
                self.fields_tree.insert(Str::from(*def.name), field_def);
            }
            Entry::Occupied(e) => {
                self.errors.redefined(Token::Field, def.name, e.get().name);
            }
        }
    }

    fn add_args_def(&mut self, def: parse::ArgsDef<'src>) {
        match self.args.entry(def.name.fragment()) {
            Entry::Vacant(e) => {
                let args = ArgsDef {
                    name: def.name,
                    is_extern: def.is_extern,
                    items: def
                        .args
                        .iter()
                        .map(|i| ArgsValue {
                            name: i.name,
                            ty: i.ty,
                            kind: None,
                        })
                        .collect(),
                };
                e.insert(args);
            }
            Entry::Occupied(e) => {
                self.errors.redefined(Token::Args, def.name, e.get().name);
            }
        }
    }

    fn create_pattern(
        &mut self,
        def: &parse::PatternDef<'src>,
        is_format: bool,
    ) -> Pattern<'src, I> {
        use parse::PatternItem as E;

        let mut pat = Pattern {
            #[cfg(feature = "raw")]
            raw: def.raw,
            name: def.name,
            mask: I::zero(),
            opcode: I::zero(),
            size: 0,
            args: vec![],
            cond: vec![],
        };

        for i in def.items.iter() {
            match i {
                E::FixedBits(..) | E::FixedField(..) => {}
                E::ArgsRef(name, set) => {
                    if let Some(r) = self.args.get(set.fragment()) {
                        pat.push_args(Value::new_set(*name, *set, r.items.clone()));
                    } else {
                        self.errors.undefined(*set, Token::Args);
                    }
                }
                E::FieldRef(i) => {
                    if let Some(field) = self.fields.get(i.field.fragment()) {
                        let field = Field::FieldRef(field.clone());
                        pat.push_args(Value::new_field(i.name, field));
                    } else {
                        self.errors.undefined(i.field, Token::Field);
                    }
                }
                E::Const(i) => {
                    pat.push_args(Value::new_const(i.name, i.value()));
                }
                E::FormatRef(r) => {
                    if let Some(format) = self.formats.get(r.fragment()) {
                        pat.mask = pat.mask.bit_or(&format.mask);
                        pat.opcode = pat.opcode.bit_or(&format.opcode);
                        for i in &format.args {
                            pat.push_args(i.clone());
                        }
                        for i in &format.cond {
                            pat.push_condition(i.clone());
                        }
                    } else {
                        self.errors.undefined(*r, Token::Format);
                    }
                }
            }
        }

        for i in def.items.iter().rev() {
            match i {
                E::FixedBits(i) => {
                    for c in i.chars().rev() {
                        pat.mask.set_bit(pat.size, c == '0' || c == '1');
                        pat.opcode.set_bit(pat.size, c == '1');
                        pat.size += 1;
                    }
                }
                E::FixedField(i) => {
                    let field = UnnamedField {
                        pos: pat.size,
                        len: i.len(),
                        sxt: i.sign_extend(),
                    };
                    pat.push_args(Value::new_field(i.name, Field::Field(field)));
                    pat.size += i.len();
                }
                _ => {}
            }
        }

        if pat.size > self.insn_size_max {
            self.errors.overflow(def.name, self.insn_size_max, pat.size);
        } else if !self.insn_size.contains(&pat.size) {
            self.errors
                .insn_size(def.name, self.insn_size.clone(), pat.size);
        }

        if !is_format {
            for arg in &pat.args {
                if let ValueKind::Set(_, args) = &arg.kind {
                    for i in args.iter().filter(|i| i.kind.is_none()) {
                        self.errors
                            .push(arg.name, ErrorKind::UndefinedMember(def.name, i.name));
                    }
                }
            }
        }

        for i in &def.cond {
            pat.push_condition(i.clone());
        }

        pat
    }

    fn add_format_def(&mut self, def: parse::FormatDef<'src>) {
        let pattern = self.create_pattern(&def, true);
        match self.formats.entry(def.name.fragment()) {
            Entry::Vacant(e) => {
                e.insert(pattern);
            }
            Entry::Occupied(e) => {
                self.errors.redefined(Token::Format, def.name, e.get().name);
            }
        }
    }

    fn add_pattern_def(&mut self, def: parse::PatternDef<'src>) {
        self.root.items.push(parse::GroupItem::PatternDef(def));
    }

    fn add_group(&mut self, group: parse::Group<'src>) {
        self.root.push_group(Box::new(group));
    }

    fn create_overlap_group(&mut self, group: &parse::Group<'src>) -> Overlap<'src, I> {
        use parse::GroupItem as E;

        assert!(group.overlap);

        let mut mask = I::ones();
        let mut opcode = I::zero();
        let mut items = vec![];

        for i in group.items.iter() {
            let e = match i {
                E::PatternDef(def) => {
                    let p = self.create_pattern(def, false);
                    mask = mask.bit_and(&p.mask);
                    opcode = opcode.bit_or(&p.opcode);
                    OverlapItem::Pattern(p)
                }
                E::Group(group) => {
                    let g = self.create_group(group);
                    mask = mask.bit_and(&g.mask);
                    OverlapItem::Group(Box::new(g))
                }
            };
            items.push(e);
        }

        let mut mask = I::ones();
        for a in items.iter() {
            match a {
                OverlapItem::Pattern(a) => {
                    mask = mask.bit_and(&a.mask);
                }
                OverlapItem::Group(a) => {
                    mask = mask.bit_and(&a.mask);
                }
            }
        }

        let mut first = true;
        let mut opcode = I::zero();
        for i in items.iter() {
            match i {
                OverlapItem::Pattern(pat) => {
                    let opc = pat.opcode.bit_and(&mask);
                    if first {
                        opcode = opc;
                        first = false;
                    } else if opcode != opc {
                        self.errors.invalid_opcode(pat.name);
                    }
                }
                OverlapItem::Group(group) => {
                    for i in &group.items {
                        let opc = i.opcode().bit_and(&mask);
                        if first {
                            opcode = opc;
                            first = false;
                        } else if opcode != opc {
                            self.errors.invalid_opcode(i.first_pattern().name);
                        }
                    }
                }
            }
        }

        opcode = opcode.bit_and(&mask);

        Overlap {
            mask,
            opcode,
            items,
        }
    }

    fn create_group(&mut self, group: &parse::Group<'src>) -> Group<'src, I> {
        use parse::GroupItem as E;

        assert!(!group.overlap);

        let mut mask = I::ones();
        let mut items = vec![];

        for i in group.items.iter() {
            let e = match i {
                E::PatternDef(def) => {
                    let p = self.create_pattern(def, false);
                    mask = mask.bit_and(&p.mask);
                    Item::Pattern(p)
                }
                E::Group(group) => {
                    let g = self.create_overlap_group(group);
                    mask = mask.bit_and(&g.mask);
                    Item::Overlap(Box::new(g))
                }
            };
            items.push(e);
        }

        for (i, a) in items.iter().enumerate() {
            let a = a.first_pattern();
            for b in items.iter().skip(i + 1) {
                let b = b.first_pattern();
                let mask = a.mask.bit_and(&b.mask);
                if a.opcode.bit_and(&mask) == b.opcode.bit_and(&mask) {
                    self.errors.overlap(b.name, a.name);
                }
            }
        }

        Group {
            mask,
            opcode: I::zero(),
            items,
        }
    }

    fn convert_field_def(&self, field: &FieldDef<'src>) -> FieldDef<S> {
        FieldDef {
            name: S::from(&field.name),
            func: field.func.map(|s| S::from(&s)),
            items: field
                .items
                .iter()
                .map(|i| self.convert_field_item(i))
                .collect(),
        }
    }

    fn convert_field_ref(&self, field_ref: &FieldRef<'src>) -> FieldRef<S> {
        let name = field_ref.field.name.fragment();
        let field = self.fields_tree.get(*name).unwrap().clone();
        FieldRef::new(field, field_ref.len, field_ref.sxt)
    }

    fn convert_field_item(&self, item: &FieldItem<'src>) -> FieldItem<S> {
        match item {
            FieldItem::Field(f) => FieldItem::Field(*f),
            FieldItem::FieldRef(f) => FieldItem::FieldRef(self.convert_field_ref(f)),
        }
    }

    fn convert_field(&self, field: &Field<'src>) -> Field<S> {
        match field {
            Field::Field(unnamed) => Field::Field(*unnamed),
            Field::FieldRef(f) => {
                Field::FieldRef(self.fields_tree.get(*f.name.fragment()).unwrap().clone())
            }
        }
    }

    fn convert_args_value_kind(&self, kind: &ArgsValueKind<'src>) -> ArgsValueKind<S> {
        match kind {
            ArgsValueKind::Field(field) => ArgsValueKind::Field(self.convert_field(field)),
            ArgsValueKind::Const(value) => ArgsValueKind::Const(*value),
        }
    }

    fn convert_arg(&self, arg: &ArgsValue<'src>) -> ArgsValue<S> {
        ArgsValue {
            name: S::from(&arg.name),
            ty: arg.ty.map(|s| S::from(&s)),
            kind: arg.kind.as_ref().map(|i| self.convert_args_value_kind(i)),
        }
    }

    fn convert_value_kind(&self, kind: &ValueKind<'src>) -> ValueKind<S> {
        match kind {
            ValueKind::Set(set, args) => ValueKind::Set(
                S::from(set),
                args.iter().map(|i| self.convert_arg(i)).collect(),
            ),
            ValueKind::Field(field) => ValueKind::Field(self.convert_field(field)),
            ValueKind::Const(value) => ValueKind::Const(*value),
        }
    }

    fn convert_value(&self, value: &Value<'src>) -> Value<S> {
        Value {
            name: S::from(&value.name),
            kind: self.convert_value_kind(&value.kind),
        }
    }

    fn convert_pattern(&self, pat: &Pattern<'src, I>) -> Pattern<I, S> {
        Pattern {
            #[cfg(feature = "raw")]
            raw: S::from(&pat.raw),
            name: S::from(&pat.name),
            mask: pat.mask,
            opcode: pat.opcode,
            size: pat.size,
            args: pat.args.iter().map(|i| self.convert_value(i)).collect(),
            cond: pat
                .cond
                .iter()
                .map(|i| Cond {
                    invert: i.invert,
                    name: S::from(&i.name),
                })
                .collect(),
        }
    }

    fn convert_overlap_item(&self, item: &OverlapItem<'src, I>) -> OverlapItem<I, S> {
        match item {
            OverlapItem::Pattern(pattern) => OverlapItem::Pattern(self.convert_pattern(pattern)),
            OverlapItem::Group(group) => OverlapItem::Group(Box::new(self.convert_group(group))),
        }
    }

    fn convert_overlap(&self, overlap: &Overlap<'src, I>) -> Overlap<I, S> {
        Overlap {
            mask: overlap.mask,
            opcode: overlap.opcode,
            items: overlap
                .items
                .iter()
                .map(|i| self.convert_overlap_item(i))
                .collect(),
        }
    }

    fn convert_item(&self, item: &Item<'src, I>) -> Item<I, S> {
        match item {
            Item::Pattern(pattern) => Item::Pattern(self.convert_pattern(pattern)),
            Item::Overlap(group) => Item::Overlap(Box::new(self.convert_overlap(group))),
            Item::Group(group) => Item::Group(Box::new(self.convert_group(group))),
        }
    }

    fn convert_group(&self, group: &Group<'src, I>) -> Group<I, S> {
        let items = group.items.iter().map(|i| self.convert_item(i)).collect();

        Group {
            mask: group.mask,
            opcode: I::zero(),
            items,
        }
    }

    /// Parse `src`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use decodetree::{Parser, Item};
    /// let src = "lui .................... ..... 0110111";
    /// let tree = Parser::<u32, &str>::new(src)
    ///     .parse()
    ///     .unwrap();
    /// let patterns = tree.root().as_slice();
    /// let p = match &patterns[0] {
    ///     Item::Pattern(p) => p,
    ///     _ => panic!(),
    /// };
    /// assert_eq!(p.name(), &"lui");
    /// ```
    pub fn parse(mut self) -> Result<DecodeTree<I, S>, Errors<'src>> {
        use parse::Stmt;

        let mut cur = Span::new(self.src);
        loop {
            match parse::stmt(cur) {
                Ok(Some((tail, stmt))) => {
                    match stmt {
                        Stmt::FieldDef(def) => self.add_field_def(def),
                        Stmt::ArgsDef(def) => self.add_args_def(def),
                        Stmt::FormatDef(def) => self.add_format_def(def),
                        Stmt::PatternDef(def) => self.add_pattern_def(def),
                        Stmt::Group(def) => self.add_group(def),
                    }
                    cur = tail;
                }
                Ok(None) => break,
                Err(err) => {
                    self.errors.push_err(err);
                    break;
                }
            }
        }

        let root = mem::take(&mut self.root);
        let root = self.create_group(&root);

        if self.errors.is_empty() {
            let root = self.convert_group(&root);

            let mut fields: Vec<_> = self.fields_tree.into_values().collect();
            fields.sort_by(|a, b| a.name.cmp(&b.name));

            let mut sets: Vec<super::SetDef<S>> =
                self.args.into_values().map(|i| i.into()).collect();
            sets.sort_by(|a, b| a.name.cmp(&b.name));

            Ok(DecodeTree { fields, sets, root })
        } else {
            Err(self.errors)
        }
    }
}
