mod parse;

use std::{
    collections::hash_map::{Entry, HashMap},
    mem,
    ops::Deref,
    str,
};

use nom::Finish;

use crate::{
    error::{ErrorKind, Errors, Token},
    parser::parse::Cond,
    DecodeTree, DefaultInsn, Insn,
};

pub use parse::Span;

#[derive(Clone, Debug)]
pub enum FieldItem<'src> {
    Field {
        pos: u32,
        len: u32,
        sxt: bool,
    },
    FieldRef {
        field: Field<'src>,
        len: u32,
        sxt: bool,
    },
}

impl FieldItem<'_> {
    fn convert(&self) -> super::FieldItem {
        use super::FieldItem as E;
        match self {
            Self::Field { pos, len, sxt } => E::field(*pos, *len, *sxt),
            Self::FieldRef { field, len, sxt } => E::field_ref(field.to_field_def(), *len, *sxt),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Field<'src> {
    pub name: Option<Span<'src>>,
    pub func: Option<Span<'src>>,
    pub items: Vec<FieldItem<'src>>,
}

impl Field<'_> {
    fn to_field_def(&self) -> super::FieldDef {
        super::FieldDef {
            name: self.name.unwrap().to_string(),
            func: self.func.map(|s| s.to_string()),
            items: self.items.iter().map(|i| i.convert()).collect(),
        }
    }

    fn to_field(&self) -> super::Field {
        if self.name.is_some() {
            super::Field::FieldRef(self.to_field_def())
        } else if let Some(FieldItem::Field { pos, len, sxt }) = self.items.first() {
            super::Field::Field(super::UnnamedField {
                pos: *pos,
                len: *len,
                sxt: *sxt,
            })
        } else {
            panic!()
        }
    }
}

#[derive(Clone, Debug)]
pub struct Arg<'src> {
    pub name: Span<'src>,
    pub ty: Option<Span<'src>>,
    pub value: Option<ValueKind<'src>>,
}

impl Arg<'_> {
    fn convert(&self) -> super::ArgsValue {
        super::ArgsValue {
            name: self.name.to_string(),
            ty: self.ty.map(|s| s.to_string()),
            kind: self
                .value
                .as_ref()
                .expect("parser must generate error")
                .convert(),
        }
    }
}

impl<'src> From<&parse::Arg<'src>> for Arg<'src> {
    fn from(value: &parse::Arg<'src>) -> Self {
        Self {
            name: value.name,
            ty: value.ty,
            value: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Args<'src> {
    pub name: Span<'src>,
    pub is_extern: bool,
    pub items: Vec<Arg<'src>>,
}

impl Args<'_> {
    fn as_args_def(&self) -> super::ArgsDef {
        super::ArgsDef {
            name: self.name.to_string(),
            is_extern: self.is_extern,
            items: self
                .items
                .iter()
                .map(|i| super::ArgDef {
                    name: i.name.to_string(),
                    ty: i.ty.map(|s| s.to_string()),
                })
                .collect(),
        }
    }
}

impl<'src> From<&parse::ArgsDef<'src>> for Args<'src> {
    fn from(value: &parse::ArgsDef<'src>) -> Self {
        Self {
            name: value.name,
            is_extern: value.is_extern,
            items: value.args.iter().map(Arg::from).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueKind<'src> {
    Set(Args<'src>),
    Field(Field<'src>),
    Const(i64),
}

impl ValueKind<'_> {
    fn convert(&self) -> super::ArgsValueKind {
        use super::ArgsValueKind as E;
        match self {
            Self::Set(_) => todo!("nested arg sets"),
            Self::Field(field) => E::Field(field.to_field()),
            Self::Const(value) => E::Const(*value),
        }
    }

    fn is_field(&self) -> bool {
        matches!(self, ValueKind::Field(..))
    }

    fn is_const(&self) -> bool {
        matches!(self, ValueKind::Const(..))
    }
}

#[derive(Clone, Debug)]
pub struct Value<'src> {
    pub name: Span<'src>,
    pub kind: ValueKind<'src>,
}

impl<'src> Value<'src> {
    fn new_set(name: Span<'src>, args: Args<'src>) -> Self {
        Self {
            name,
            kind: ValueKind::Set(args),
        }
    }

    fn new_field(name: Span<'src>, field: Field<'src>) -> Self {
        Self {
            name,
            kind: ValueKind::Field(field),
        }
    }

    fn new_const(name: Span<'src>, value: i64) -> Self {
        Self {
            name,
            kind: ValueKind::Const(value),
        }
    }

    fn as_pattern_value(&self) -> super::Value {
        use super::ValueKind as E;

        super::Value {
            name: self.name.to_string(),
            kind: match &self.kind {
                ValueKind::Set(args) => {
                    let args = args.items.iter().map(|i| i.convert()).collect();
                    E::Args(args)
                }
                ValueKind::Field(field) => E::Field(field.to_field()),
                ValueKind::Const(value) => E::Const(*value),
            },
        }
    }
}

impl<'src> Deref for Value<'src> {
    type Target = ValueKind<'src>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

#[derive(Clone, Debug)]
pub struct Pattern<'src, I = DefaultInsn> {
    pub mask: I,
    pub opcode: I,
    pub name: Span<'src>,
    pub args: Vec<Value<'src>>,
    pub cond: Vec<Cond<'src>>,
}

impl<I> Pattern<'_, I> {
    fn convert(self) -> super::Pattern<I> {
        super::Pattern {
            mask: self.mask,
            opcode: self.opcode,
            name: self.name.to_string(),
            args: self.args.iter().map(|i| i.as_pattern_value()).collect(),
            cond: self
                .cond
                .into_iter()
                .map(|i| super::Cond {
                    invert: i.invert,
                    name: i.name.to_string(),
                })
                .collect(),
        }
    }
}

impl<'src, I: Insn> Pattern<'src, I> {
    fn args_push(&mut self, value: Value<'src>) {
        match &value.kind {
            ValueKind::Set(..) => self.args.push(value),
            ValueKind::Field(..) | ValueKind::Const(..) => {
                // fill empty slot
                for arg in self.args.iter_mut().rev() {
                    if let ValueKind::Set(set) = &mut arg.kind {
                        if let Some(item) = set.items.iter_mut().find(|i| {
                            i.value.is_none() && i.name.fragment() == value.name.fragment()
                        }) {
                            item.value = Some(value.kind);
                            return;
                        }
                    }
                }

                // override slot in last set
                for arg in self.args.iter_mut().rev() {
                    if let ValueKind::Set(set) = &mut arg.kind {
                        if let Some(item) = set
                            .items
                            .iter_mut()
                            .find(|i| i.name.fragment() == value.name.fragment())
                        {
                            item.value = Some(value.kind);
                            return;
                        }
                    }
                }

                // remove last field/const with the same name
                if let Some(i) = self.args.iter().position(|i| {
                    (i.is_field() || i.is_const()) && i.name.fragment() == value.name.fragment()
                }) {
                    self.args.remove(i);
                }

                self.args.push(value);
            }
        }
    }

    fn cond_push(&mut self, cond: Cond<'src>) {
        if let Some(prev) = self.cond.iter_mut().find(|i| i.name == cond.name) {
            prev.invert = cond.invert;
        } else {
            self.cond.push(cond.clone());
        }
    }
}

#[derive(Clone, Debug)]
pub enum OverlapItem<'src, I = DefaultInsn> {
    Pattern(Pattern<'src, I>),
    Group(Box<Group<'src, I>>),
}

impl<I> OverlapItem<'_, I> {
    fn convert(self) -> super::OverlapItem<I> {
        match self {
            Self::Pattern(pattern) => super::OverlapItem::Pattern(pattern.convert()),
            Self::Group(group) => super::OverlapItem::Group(Box::new(group.convert())),
        }
    }
}

impl<'src, I> OverlapItem<'src, I> {
    fn first_pattern(&self) -> &Pattern<'src, I> {
        match self {
            OverlapItem::Pattern(pattern) => pattern,
            OverlapItem::Group(group) => group.first_pattern(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Overlap<'src, I = DefaultInsn> {
    pub mask: I,
    pub opcode: I,
    pub items: Vec<OverlapItem<'src, I>>,
}

impl<I> Overlap<'_, I> {
    fn convert(self) -> super::Overlap<I> {
        super::Overlap {
            mask: self.mask,
            opcode: self.opcode,
            items: self.items.into_iter().map(|i| i.convert()).collect(),
        }
    }
}

impl<'src, I> Overlap<'src, I> {
    fn first_pattern(&self) -> &Pattern<'src, I> {
        self.items
            .first()
            .expect("group must not be empty")
            .first_pattern()
    }
}

#[derive(Clone, Debug, Default)]
pub struct Group<'src, I = DefaultInsn> {
    pub mask: I,
    pub items: Vec<Item<'src, I>>,
}

impl<I> Group<'_, I> {
    fn convert(self) -> super::Group<I> {
        super::Group {
            mask: self.mask,
            items: self.items.into_iter().map(|i| i.convert()).collect(),
        }
    }
}

impl<'src, I> Group<'src, I> {
    fn first_pattern(&self) -> &Pattern<'src, I> {
        self.items
            .first()
            .expect("group must not be empty")
            .first_pattern()
    }
}

#[derive(Clone, Debug)]
pub enum Item<'src, I> {
    Pattern(Pattern<'src, I>),
    Overlap(Box<Overlap<'src, I>>),
}

impl<I: Copy> Item<'_, I> {
    pub fn opcode(&self) -> I {
        match self {
            Self::Pattern(i) => i.opcode,
            Self::Overlap(i) => i.opcode,
        }
    }
}

impl<'src, I> Item<'src, I> {
    fn first_pattern(&self) -> &Pattern<'src, I> {
        match self {
            Self::Pattern(pattern) => pattern,
            Self::Overlap(overlap) => overlap.first_pattern(),
        }
    }
}

impl<I> Item<'_, I> {
    fn convert(self) -> super::Item<I> {
        match self {
            Self::Pattern(pattern) => super::Item::Pattern(pattern.convert()),
            Self::Overlap(group) => super::Item::Overlap(Box::new(group.convert())),
        }
    }
}

pub struct Parser<'src, T = super::DefaultInsn> {
    src: &'src str,
    insn_size: u32,
    is_fixed_insn: bool,
    fields: HashMap<&'src str, Field<'src>>,
    args: HashMap<&'src str, Args<'src>>,
    formats: HashMap<&'src str, Pattern<'src, T>>,
    root: parse::Group<'src>,
    errors: Errors<'src>,
}

impl<'src, I> Parser<'src, I>
where
    I: Insn,
{
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            insn_size: I::width(),
            is_fixed_insn: false,
            errors: Errors::new(src),
            fields: Default::default(),
            args: Default::default(),
            formats: Default::default(),
            root: Default::default(),
        }
    }

    pub fn set_insn_size(mut self, size: u32) -> Self {
        self.insn_size = size;
        self
    }

    pub fn set_insn_fixed_size(mut self, is_fixed_insn: bool) -> Self {
        self.is_fixed_insn = is_fixed_insn;
        self
    }

    fn check_field_range(&mut self, pos: parse::Number<'src, u32>, len: parse::Number<'src, u32>) {
        if len.value == 0 {
            self.errors.push(len.span, ErrorKind::FieldLenZero);
        }
        if pos.value >= self.insn_size {
            self.errors.field_pos(pos.span, self.insn_size, pos.value);
        } else if pos.value + len.value > self.insn_size {
            self.errors
                .field_pos(len.span, self.insn_size, pos.value + len.value);
        }
    }

    fn add_field_def(&mut self, def: parse::FieldDef<'src>) {
        use parse::Field as E;

        let mut items = Vec::with_capacity(def.items.len());

        for i in &def.items {
            match i {
                E::Unnamed(field) => {
                    self.check_field_range(field.pos, field.len);
                    let item = FieldItem::Field {
                        pos: field.pos(),
                        len: field.len(),
                        sxt: field.sign_extend(),
                    };
                    items.push(item);
                }
                E::Named(r) => {
                    if let Some(field) = self.fields.get(r.name.fragment()) {
                        let item = FieldItem::FieldRef {
                            field: field.clone(),
                            len: r.len(),
                            sxt: r.sign_extend(),
                        };
                        items.push(item);
                    } else {
                        self.errors.undefined(r.name, Token::Field);
                    }
                }
            };
        }

        match self.fields.entry(def.name.fragment()) {
            Entry::Vacant(e) => {
                let field = Field {
                    name: Some(def.name),
                    func: def.func,
                    items,
                };
                e.insert(field);
            }
            Entry::Occupied(e) => {
                if let Some(name) = e.get().name {
                    self.errors.redefined(Token::Field, def.name, name);
                }
            }
        }
    }

    fn add_args_def(&mut self, def: parse::ArgsDef<'src>) {
        match self.args.entry(def.name.fragment()) {
            Entry::Vacant(e) => {
                e.insert((&def).into());
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
            mask: I::zero(),
            opcode: I::zero(),
            name: def.name,
            args: vec![],
            cond: vec![],
        };

        for i in def.items.iter() {
            match i {
                E::FixedBits(..) | E::FixedField(..) => {}
                E::ArgsRef(i) => {
                    if let Some(r) = self.args.get(i.fragment()) {
                        pat.args_push(Value::new_set(*i, r.clone()));
                    } else {
                        self.errors.undefined(*i, Token::Args);
                    }
                }
                E::FieldRef(i) => {
                    if let Some(field) = self.fields.get(i.field.fragment()) {
                        pat.args_push(Value::new_field(i.name, field.clone()));
                    } else {
                        self.errors.undefined(i.field, Token::Field);
                    }
                }
                E::Const(i) => {
                    pat.args_push(Value::new_const(i.name, i.value()));
                }
                E::FormatRef(r) => {
                    if let Some(format) = self.formats.get(r.fragment()) {
                        pat.mask = pat.mask.bit_or(&format.mask);
                        pat.opcode = pat.opcode.bit_or(&format.opcode);
                        for i in &format.args {
                            pat.args_push(i.clone());
                        }
                        for i in &format.cond {
                            pat.cond_push(i.clone());
                        }
                    } else {
                        self.errors.undefined(*r, Token::Format);
                    }
                }
            }
        }

        let mut pos = 0;
        for i in def.items.iter().rev() {
            match i {
                E::FixedBits(i) => {
                    for c in i.chars().rev() {
                        pat.mask.set_bit(pos, c == '0' || c == '1');
                        pat.opcode.set_bit(pos, c == '1');
                        pos += 1;
                    }
                }
                E::FixedField(i) => {
                    let field = Field {
                        name: None,
                        func: None,
                        items: vec![FieldItem::Field {
                            pos,
                            len: i.len(),
                            sxt: i.sign_extend(),
                        }],
                    };
                    pat.args_push(Value::new_field(i.name, field));
                    pos += i.len();
                }
                _ => {}
            }
        }

        if pos > self.insn_size {
            self.errors.overflow(def.name, self.insn_size, pos);
        } else if pos == 0 || (self.is_fixed_insn && pos != self.insn_size) {
            self.errors.insn_size(def.name, self.insn_size, pos);
        }

        if !is_format {
            for arg in &pat.args {
                if let ValueKind::Set(ref set) = arg.kind {
                    for i in set.items.iter().filter(|i| i.value.is_none()) {
                        self.errors
                            .push(arg.name, ErrorKind::UndefinedMember(def.name, i.name));
                    }
                }
            }
        }

        for i in &def.cond {
            pat.cond_push(i.clone());
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

        Group { mask, items }
    }

    pub fn parse(mut self) -> Result<DecodeTree<I>, Errors<'src>> {
        use parse::Stmt;

        let mut cur = Span::new(self.src);
        while !cur.is_empty() {
            match parse::stmt(cur).finish() {
                Ok((tail, stmt)) => {
                    match stmt {
                        Stmt::FieldDef(def) => self.add_field_def(def),
                        Stmt::ArgsDef(def) => self.add_args_def(def),
                        Stmt::FormatDef(def) => self.add_format_def(def),
                        Stmt::PatternDef(def) => self.add_pattern_def(def),
                        Stmt::Group(def) => self.add_group(def),
                    }
                    cur = tail;
                }
                Err(err) => {
                    self.errors.push_err(err);
                    break;
                }
            }
        }

        let root = mem::take(&mut self.root);
        let root = self.create_group(&root);

        if self.errors.is_empty() {
            let fields = self
                .fields
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_field_def()))
                .collect();

            let args = self
                .args
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.as_args_def()))
                .collect();

            let root = root.convert();

            Ok(DecodeTree { fields, args, root })
        } else {
            Err(self.errors)
        }
    }
}
