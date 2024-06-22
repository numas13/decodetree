mod error;
mod parser;

#[cfg(feature = "gen")]
pub mod gen;

use std::{collections::HashMap, fmt::LowerHex, hash::Hash, mem};

use crate::parser::Span;

pub use crate::error::{ErrorPrinter, Errors};
pub use crate::parser::Parser;

#[cfg(feature = "gen")]
pub use crate::gen::Generator;

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

impl From<&parser::Arg<'_>> for Arg {
    fn from(value: &parser::Arg<'_>) -> Self {
        Self {
            name: value.name().to_owned(),
            ty: value.ty().map(str::to_owned),
            value: None,
        }
    }
}

impl<'a> From<&parser::Arg<'a>> for Arg<Span<'a>> {
    fn from(value: &parser::Arg<'a>) -> Self {
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

impl From<&parser::ArgsDef<'_>> for Args {
    fn from(value: &parser::ArgsDef<'_>) -> Self {
        Self {
            name: value.name.to_string(),
            is_extern: value.is_extern,
            items: value.args.iter().map(Arg::from).collect(),
        }
    }
}

impl<'a> From<&parser::ArgsDef<'a>> for Args<Span<'a>> {
    fn from(value: &parser::ArgsDef<'a>) -> Self {
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
pub struct Cond<S = String> {
    pub invert: bool,
    pub name: S,
}

impl Cond<Span<'_>> {
    fn convert(self) -> Cond {
        Cond {
            invert: self.invert,
            name: self.name.to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Pattern<I, S = String> {
    pub mask: I,
    pub opcode: I,
    pub name: S,
    pub args: Vec<Value<S>>,
    pub cond: Vec<Cond<S>>,
}

impl<I> Pattern<I, Span<'_>> {
    fn convert(self) -> Pattern<I> {
        Pattern {
            mask: self.mask,
            opcode: self.opcode,
            name: self.name.to_string(),
            args: self.args.into_iter().map(|i| i.convert()).collect(),
            cond: self.cond.into_iter().map(|i| i.convert()).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum OverlapItem<I, S = String> {
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
pub struct Overlap<I, S = String> {
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
pub struct Group<I, S = String> {
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

#[derive(Clone, Debug, Default)]
pub struct DecodeTree<I> {
    pub fields: HashMap<String, Field>,
    pub args: HashMap<String, Args>,
    pub root: Group<I>,
}

pub fn parse<T>(src: &str) -> Result<DecodeTree<T>, Errors>
where
    T: Insn,
{
    Parser::new(src).parse()
}
