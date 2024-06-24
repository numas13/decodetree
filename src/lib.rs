mod error;
mod parser;

#[cfg(feature = "gen")]
pub mod gen;

use std::{fmt::LowerHex, hash::Hash, mem, ops::Deref, rc::Rc};

use crate::parser::Span;

pub use crate::error::{ErrorPrinter, Errors};
pub use crate::parser::Parser;

#[cfg(feature = "gen")]
pub use crate::gen::Generator;

type DefaultInsn = u32;

/// Default type for storing strings.
pub type Str = Box<str>;

pub trait Insn: Sized + Copy + Clone + Eq + Ord + Hash + LowerHex + Default {
    // TODO: zero_extract
    // TODO: sign_extract
    fn width() -> u32;
    fn zero() -> Self;
    fn ones() -> Self;
    fn set_bit(&mut self, offset: u32, bit: bool);
    fn bit_not(&self) -> Self;
    fn bit_and(&self, other: &Self) -> Self;
    fn bit_andn(&self, other: &Self) -> Self;
    fn bit_or(&self, other: &Self) -> Self;
}

macro_rules! impl_insn {
    ($($t:ty),+ $(,)*) => (
        $(impl Insn for $t {
            fn width() -> u32 {
                mem::size_of::<Self>() as u32 * 8
            }

            fn zero() -> Self {
                0
            }

            fn ones() -> Self {
                !0
            }

            fn set_bit(&mut self, offset: u32, bit: bool) {
                if bit && offset < Self::width() {
                    *self |= (bit as $t) << offset;
                }
            }

            fn bit_not(&self) -> Self {
                !*self
            }

            fn bit_and(&self, other: &Self) -> Self {
                *self & *other
            }

            fn bit_andn(&self, other: &Self) -> Self {
                *self & !*other
            }

            fn bit_or(&self, other: &Self) -> Self {
                *self | *other
            }
        })+
    );
}

impl_insn!(u8, u16, u32, u64, u128);

#[derive(Copy, Clone, Debug)]
pub struct UnnamedField {
    pos: u32,
    len: u32,
    sxt: bool,
}

impl UnnamedField {
    pub fn pos(&self) -> u32 {
        self.pos
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn sxt(&self) -> bool {
        self.sxt
    }
}

#[derive(Clone, Debug)]
pub struct FieldRef<S = Str> {
    field: Rc<FieldDef<S>>,
    len: u32,
    sxt: bool,
}

impl<S> FieldRef<S> {
    fn new(field: Rc<FieldDef<S>>, len: u32, sxt: bool) -> Self {
        Self { field, len, sxt }
    }

    pub fn field(&self) -> &FieldDef<S> {
        &self.field
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn sxt(&self) -> bool {
        self.sxt
    }
}

#[derive(Clone, Debug)]
pub enum FieldItem<S = Str> {
    Field(UnnamedField),
    FieldRef(FieldRef<S>),
}

impl<S> FieldItem<S> {
    fn field(pos: u32, len: u32, sxt: bool) -> Self {
        Self::Field(UnnamedField { pos, len, sxt })
    }

    fn field_ref(field: Rc<FieldDef<S>>, len: u32, sxt: bool) -> Self {
        Self::FieldRef(FieldRef { field, len, sxt })
    }
}

#[derive(Clone, Debug)]
pub struct FieldDef<S = Str> {
    name: S,
    func: Option<S>,
    items: Vec<FieldItem<S>>,
}

impl<S> FieldDef<S> {
    pub fn name(&self) -> &S {
        &self.name
    }

    pub fn func(&self) -> Option<&S> {
        self.func.as_ref()
    }

    pub fn items(&self) -> &[FieldItem<S>] {
        &self.items
    }

    pub fn iter(&self) -> impl Iterator<Item = &FieldItem<S>> {
        self.items.iter()
    }
}

#[derive(Clone, Debug)]
pub struct ArgDef<S = Str> {
    name: S,
    ty: Option<S>,
}

impl<S> ArgDef<S> {
    pub fn name(&self) -> &S {
        &self.name
    }

    pub fn ty(&self) -> Option<&S> {
        self.ty.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct ArgsDef<S = Str> {
    name: S,
    is_extern: bool,
    items: Vec<ArgDef<S>>,
}

impl<S> ArgsDef<S> {
    pub fn name(&self) -> &S {
        &self.name
    }

    pub fn is_extern(&self) -> bool {
        self.is_extern
    }

    pub fn items(&self) -> &[ArgDef<S>] {
        self.items.as_slice()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ArgDef<S>> {
        self.items.iter()
    }
}

#[derive(Clone, Debug)]
pub enum Field<S = Str> {
    Field(UnnamedField),
    FieldRef(Rc<FieldDef<S>>),
}

#[derive(Clone, Debug)]
pub enum ArgsValueKind<S = Str> {
    Const(i64),
    Field(Field<S>),
}

#[derive(Clone, Debug)]
pub struct ArgsValue<S = Str> {
    name: S,
    ty: Option<S>,
    kind: Option<ArgsValueKind<S>>,
}

impl<S> ArgsValue<S> {
    pub fn name(&self) -> &S {
        &self.name
    }

    pub fn ty(&self) -> Option<&S> {
        self.ty.as_ref()
    }

    pub fn kind(&self) -> &ArgsValueKind<S> {
        self.kind.as_ref().expect("handled by parser")
    }
}

#[derive(Clone, Debug)]
pub enum ValueKind<S = Str> {
    Const(i64),
    Field(Field<S>),
    Args(Vec<ArgsValue<S>>),
}

#[derive(Clone, Debug)]
pub struct Value<S = Str> {
    name: S,
    kind: ValueKind<S>,
}

impl<S> Value<S> {
    pub fn name(&self) -> &S {
        &self.name
    }

    fn new_const(name: S, value: i64) -> Self {
        Self {
            name,
            kind: ValueKind::Const(value),
        }
    }

    fn new_field(name: S, field: Field<S>) -> Self {
        Self {
            name,
            kind: ValueKind::Field(field),
        }
    }

    fn new_set(name: S, args: Vec<ArgsValue<S>>) -> Self {
        Self {
            name,
            kind: ValueKind::Args(args),
        }
    }

    pub fn kind(&self) -> &ValueKind<S> {
        &self.kind
    }

    pub fn is_set(&self) -> bool {
        matches!(self.kind, ValueKind::Args(..))
    }

    pub fn is_field(&self) -> bool {
        matches!(self.kind, ValueKind::Field(..))
    }

    pub fn is_const(&self) -> bool {
        matches!(self.kind, ValueKind::Const(..))
    }
}

impl<S> Deref for Value<S> {
    type Target = ValueKind<S>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cond<S = Str> {
    name: S,
    invert: bool,
}

impl<S> Cond<S> {
    pub fn name(&self) -> &S {
        &self.name
    }

    pub fn invert(&self) -> bool {
        self.invert
    }
}

#[derive(Clone, Debug)]
pub struct Pattern<I = DefaultInsn, S = Str> {
    name: S,
    mask: I,
    opcode: I,
    size: u32,
    args: Vec<Value<S>>,
    cond: Vec<Cond<S>>,
}

impl<I, S> Pattern<I, S> {
    pub fn name(&self) -> &S {
        &self.name
    }

    pub fn mask(&self) -> &I {
        &self.mask
    }

    pub fn opcode(&self) -> &I {
        &self.opcode
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn size(&self) -> u32 {
        self.size
    }

    pub fn args(&self) -> &[Value<S>] {
        self.args.as_slice()
    }

    pub fn conditions(&self) -> &[Cond<S>] {
        self.cond.as_slice()
    }
}

impl<'a, I> Pattern<I, Span<'a>> {
    fn args_push(&mut self, value: Value<Span<'a>>) {
        fn convert(kind: ValueKind<Span>) -> ArgsValueKind<Span> {
            match kind {
                ValueKind::Const(value) => ArgsValueKind::Const(value),
                ValueKind::Field(field) => ArgsValueKind::Field(field),
                ValueKind::Args(..) => panic!("nested args set"),
            }
        }

        match &value.kind {
            ValueKind::Args(..) => self.args.push(value),
            ValueKind::Field(..) | ValueKind::Const(..) => {
                // fill empty slot
                for arg in self.args.iter_mut().rev() {
                    if let ValueKind::Args(args) = &mut arg.kind {
                        if let Some(item) = args.iter_mut().find(|i| {
                            i.kind.is_none() && i.name.fragment() == value.name.fragment()
                        }) {
                            item.kind = Some(convert(value.kind));
                            return;
                        }
                    }
                }

                // override slot in last set
                for arg in self.args.iter_mut().rev() {
                    if let ValueKind::Args(args) = &mut arg.kind {
                        if let Some(item) = args
                            .iter_mut()
                            .find(|i| i.name.fragment() == value.name.fragment())
                        {
                            item.kind = Some(convert(value.kind));
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
}

impl<I, S> Pattern<I, S>
where
    S: Clone + Eq,
{
    fn cond_push(&mut self, cond: Cond<S>) {
        if let Some(prev) = self.cond.iter_mut().find(|i| i.name == cond.name) {
            prev.invert = cond.invert;
        } else {
            self.cond.push(cond.clone());
        }
    }
}

#[derive(Clone, Debug)]
pub enum OverlapItem<I = DefaultInsn, S = Str> {
    Pattern(Pattern<I, S>),
    Group(Box<Group<I, S>>),
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
pub struct Overlap<I = DefaultInsn, S = Str> {
    mask: I,
    opcode: I,
    items: Vec<OverlapItem<I, S>>,
}

impl<I, S> Overlap<I, S> {
    pub fn mask(&self) -> &I {
        &self.mask
    }

    pub fn opcode(&self) -> &I {
        &self.opcode
    }

    pub fn as_slice(&self) -> &[OverlapItem<I, S>] {
        self.items.as_slice()
    }

    pub fn iter(&self) -> impl Iterator<Item = &OverlapItem<I, S>> {
        self.items.iter()
    }

    fn first_pattern(&self) -> &Pattern<I, S> {
        self.items
            .first()
            .expect("group must not be empty")
            .first_pattern()
    }
}

#[derive(Clone, Debug)]
pub enum Item<I, S = Str> {
    Pattern(Pattern<I, S>),
    Overlap(Box<Overlap<I, S>>),
}

impl<I, S> Item<I, S> {
    pub fn opcode(&self) -> &I {
        match self {
            Self::Pattern(i) => &i.opcode,
            Self::Overlap(i) => &i.opcode,
        }
    }

    pub fn mask(&self) -> &I {
        match self {
            Self::Pattern(i) => &i.mask,
            Self::Overlap(i) => &i.mask,
        }
    }

    fn first_pattern(&self) -> &Pattern<I, S> {
        match self {
            Self::Pattern(pattern) => pattern,
            Self::Overlap(overlap) => overlap.first_pattern(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Group<I = DefaultInsn, S = Str> {
    mask: I,
    items: Vec<Item<I, S>>,
}

impl<I, S> Group<I, S> {
    pub fn mask(&self) -> &I {
        &self.mask
    }

    pub fn as_slice(&self) -> &[Item<I, S>] {
        self.items.as_slice()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Item<I, S>> {
        self.items.iter()
    }

    fn first_pattern(&self) -> &Pattern<I, S> {
        self.items
            .first()
            .expect("group must not be empty")
            .first_pattern()
    }
}

#[derive(Clone, Debug, Default)]
pub struct DecodeTree<I = DefaultInsn, S = Str> {
    pub fields: Vec<Rc<FieldDef<S>>>,
    pub args: Vec<ArgsDef<S>>,
    pub root: Group<I, S>,
}

pub fn parse<I>(src: &str) -> Result<DecodeTree<I, Str>, Errors>
where
    I: Insn,
{
    Parser::<I, Str>::new(src).parse()
}
