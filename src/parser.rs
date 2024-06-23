mod parse;

use std::{
    collections::hash_map::{Entry, HashMap},
    mem, str,
};

use nom::Finish;

use crate::{
    error::{ErrorKind, Errors, Token},
    DecodeTree, DefaultInsn, Insn,
};

pub use parse::Span;

#[derive(Clone, Debug)]
pub enum FieldItem<S = String> {
    Field {
        pos: u32,
        len: u32,
        sxt: bool,
    },
    FieldRef {
        field: Field<S>,
        len: u32,
        sxt: bool,
    },
}

impl FieldItem<Span<'_>> {
    fn convert(self) -> FieldItem {
        match self {
            Self::Field { pos, len, sxt } => FieldItem::Field { pos, len, sxt },
            Self::FieldRef { field, len, sxt } => FieldItem::FieldRef {
                field: field.convert(),
                len,
                sxt,
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct Field<S = String> {
    pub name: Option<S>,
    pub func: Option<S>,
    pub items: Vec<FieldItem<S>>,
}

impl Field<Span<'_>> {
    fn convert(self) -> Field {
        Field {
            name: self.name.map(|s| s.to_string()),
            func: self.func.map(|s| s.to_string()),
            items: self.items.into_iter().map(|i| i.convert()).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Arg<S = String> {
    pub name: S,
    pub ty: Option<S>,
    pub value: Option<ValueKind<S>>,
}

impl Arg<Span<'_>> {
    fn convert(self) -> Arg {
        Arg {
            name: self.name.to_string(),
            ty: self.ty.map(|s| s.to_string()),
            value: self.value.map(|v| v.convert()),
        }
    }
}

impl From<&parse::Arg<'_>> for Arg {
    fn from(value: &parse::Arg<'_>) -> Self {
        Self {
            name: value.name().to_owned(),
            ty: value.ty().map(str::to_owned),
            value: None,
        }
    }
}

impl<'a> From<&parse::Arg<'a>> for Arg<Span<'a>> {
    fn from(value: &parse::Arg<'a>) -> Self {
        Self {
            name: value.name,
            ty: value.ty,
            value: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Args<S = String> {
    pub name: S,
    pub is_extern: bool,
    pub items: Vec<Arg<S>>,
}

impl Args<Span<'_>> {
    fn convert(self) -> Args {
        Args {
            name: self.name.to_string(),
            is_extern: self.is_extern,
            items: self.items.into_iter().map(|i| i.convert()).collect(),
        }
    }
}

impl From<&parse::ArgsDef<'_>> for Args {
    fn from(value: &parse::ArgsDef<'_>) -> Self {
        Self {
            name: value.name.to_string(),
            is_extern: value.is_extern,
            items: value.args.iter().map(Arg::from).collect(),
        }
    }
}

impl<'a> From<&parse::ArgsDef<'a>> for Args<Span<'a>> {
    fn from(value: &parse::ArgsDef<'a>) -> Self {
        Self {
            name: value.name,
            is_extern: value.is_extern,
            items: value.args.iter().map(Arg::from).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueKind<S = String> {
    Set(Args<S>),
    Field(Field<S>),
    Const(i64),
}

impl ValueKind<Span<'_>> {
    fn convert(self) -> ValueKind {
        match self {
            Self::Set(args) => ValueKind::Set(args.convert()),
            Self::Field(field) => ValueKind::Field(field.convert()),
            Self::Const(value) => ValueKind::Const(value),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Value<S = String> {
    pub name: S,
    pub kind: ValueKind<S>,
}

impl<S> Value<S> {
    fn new_set(name: S, args: Args<S>) -> Self {
        Self {
            name,
            kind: ValueKind::Set(args),
        }
    }

    fn new_field(name: S, field: Field<S>) -> Self {
        Self {
            name,
            kind: ValueKind::Field(field),
        }
    }

    fn new_const(name: S, value: i64) -> Self {
        Self {
            name,
            kind: ValueKind::Const(value),
        }
    }

    pub fn is_set(&self) -> bool {
        matches!(self.kind, ValueKind::Set(..))
    }

    pub fn is_field(&self) -> bool {
        matches!(self.kind, ValueKind::Field(..))
    }

    pub fn is_const(&self) -> bool {
        matches!(self.kind, ValueKind::Const(..))
    }
}

impl Value<Span<'_>> {
    fn convert(self) -> Value {
        Value {
            name: self.name.to_string(),
            kind: self.kind.convert(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cond {
    pub invert: bool,
    pub name: String,
}

impl From<&'_ parse::Cond<'_>> for Cond {
    fn from(other: &parse::Cond) -> Self {
        Cond {
            invert: other.invert,
            name: other.name.to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Pattern<I = DefaultInsn, S = String> {
    pub mask: I,
    pub opcode: I,
    pub name: S,
    pub args: Vec<Value<S>>,
    pub cond: Vec<Cond>,
}

impl<I> Pattern<I, Span<'_>> {
    fn convert(self) -> Pattern<I> {
        Pattern {
            mask: self.mask,
            opcode: self.opcode,
            name: self.name.to_string(),
            args: self.args.into_iter().map(|i| i.convert()).collect(),
            cond: self.cond,
        }
    }
}

impl<'a, I: Insn> Pattern<I, Span<'a>> {
    fn args_push(&mut self, value: Value<Span<'a>>) {
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

    fn cond_push(&mut self, cond: Cond) {
        if let Some(prev) = self.cond.iter_mut().find(|i| i.name == cond.name) {
            prev.invert = cond.invert;
        } else {
            self.cond.push(Cond {
                invert: cond.invert,
                name: cond.name.to_string(),
            })
        }
    }
}

#[derive(Clone, Debug)]
pub enum OverlapItem<I = DefaultInsn, S = String> {
    Pattern(Pattern<I, S>),
    Group(Box<Group<I, S>>),
}

impl<I> OverlapItem<I, Span<'_>> {
    fn convert(self) -> OverlapItem<I> {
        match self {
            Self::Pattern(pattern) => OverlapItem::Pattern(pattern.convert()),
            Self::Group(group) => OverlapItem::Group(Box::new(group.convert())),
        }
    }
}

impl<I, S> OverlapItem<I, S> {
    fn first_pattern(&self) -> &Pattern<I, S> {
        match self {
            OverlapItem::Pattern(pattern) => pattern,
            OverlapItem::Group(group) => group.first_pattern(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Overlap<I = DefaultInsn, S = String> {
    pub mask: I,
    pub opcode: I,
    pub items: Vec<OverlapItem<I, S>>,
}

impl<I> Overlap<I, Span<'_>> {
    fn convert(self) -> Overlap<I> {
        Overlap {
            mask: self.mask,
            opcode: self.opcode,
            items: self.items.into_iter().map(|i| i.convert()).collect(),
        }
    }
}

impl<I, S> Overlap<I, S> {
    fn first_pattern(&self) -> &Pattern<I, S> {
        self.items
            .first()
            .expect("group must not be empty")
            .first_pattern()
    }
}

#[derive(Clone, Debug, Default)]
pub struct Group<I = DefaultInsn, S = String> {
    pub mask: I,
    pub items: Vec<Item<I, S>>,
}

impl<I> Group<I, Span<'_>> {
    fn convert(self) -> Group<I> {
        Group {
            mask: self.mask,
            items: self.items.into_iter().map(|i| i.convert()).collect(),
        }
    }
}

impl<I, S> Group<I, S> {
    fn first_pattern(&self) -> &Pattern<I, S> {
        self.items
            .first()
            .expect("group must not be empty")
            .first_pattern()
    }
}

#[derive(Clone, Debug)]
pub enum Item<I, S = String> {
    Pattern(Pattern<I, S>),
    Overlap(Box<Overlap<I, S>>),
}

impl<I: Copy, S> Item<I, S> {
    pub fn opcode(&self) -> I {
        match self {
            Self::Pattern(i) => i.opcode,
            Self::Overlap(i) => i.opcode,
        }
    }

    pub fn mask(&self) -> I {
        match self {
            Self::Pattern(i) => i.mask,
            Self::Overlap(i) => i.mask,
        }
    }
}

impl<I, S> Item<I, S> {
    fn first_pattern(&self) -> &Pattern<I, S> {
        match self {
            Self::Pattern(pattern) => pattern,
            Self::Overlap(overlap) => overlap.first_pattern(),
        }
    }
}

impl<I> Item<I, Span<'_>> {
    fn convert(self) -> Item<I> {
        match self {
            Self::Pattern(pattern) => Item::Pattern(pattern.convert()),
            Self::Overlap(group) => Item::Overlap(Box::new(group.convert())),
        }
    }
}

pub struct Parser<'a, T = super::DefaultInsn> {
    src: &'a str,
    insn_size: u32,
    is_fixed_insn: bool,
    fields: HashMap<&'a str, Field<Span<'a>>>,
    args: HashMap<&'a str, Args<Span<'a>>>,
    formats: HashMap<&'a str, Pattern<T, Span<'a>>>,
    root: parse::Group<'a>,
    errors: Errors<'a>,
}

impl<'a, I> Parser<'a, I>
where
    I: Insn,
{
    pub fn new(src: &'a str) -> Self {
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

    fn check_field_range(&mut self, pos: parse::Number<'a, u32>, len: parse::Number<'a, u32>) {
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

    fn add_field_def(&mut self, def: parse::FieldDef<'a>) {
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

    fn add_args_def(&mut self, def: parse::ArgsDef<'a>) {
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
        def: &parse::PatternDef<'a>,
        is_format: bool,
    ) -> Pattern<I, Span<'a>> {
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
            pat.cond_push(i.into());
        }

        pat
    }

    fn add_format_def(&mut self, def: parse::FormatDef<'a>) {
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

    fn add_pattern_def(&mut self, def: parse::PatternDef<'a>) {
        self.root.items.push(parse::GroupItem::PatternDef(def));
    }

    fn add_group(&mut self, group: parse::Group<'a>) {
        self.root.push_group(Box::new(group));
    }

    fn create_overlap_group(&mut self, group: &parse::Group<'a>) -> Overlap<I, Span<'a>> {
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

    fn create_group(&mut self, group: &parse::Group<'a>) -> Group<I, Span<'a>> {
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

    pub fn parse(mut self) -> Result<DecodeTree<I>, Errors<'a>> {
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
                .map(|(k, v)| (k.to_string(), v.convert()))
                .collect();

            let args = self
                .args
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.convert()))
                .collect();

            let root = root.convert();

            Ok(DecodeTree { fields, args, root })
        } else {
            Err(self.errors)
        }
    }
}
