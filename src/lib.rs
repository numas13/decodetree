mod error;
mod parser;

#[cfg(feature = "gen")]
pub mod gen;

use std::{collections::HashMap, fmt::LowerHex, hash::Hash, mem};

pub use crate::error::{ErrorPrinter, Errors};
pub use crate::parser::Parser;

#[cfg(feature = "gen")]
pub use crate::gen::Generator;

type DefaultInsn = u32;

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
pub struct FieldRef {
    field: FieldDef,
    len: u32,
    sxt: bool,
}

impl FieldRef {
    pub fn field(&self) -> &FieldDef {
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
pub enum FieldItem {
    Field(UnnamedField),
    FieldRef(FieldRef),
}

impl FieldItem {
    fn field(pos: u32, len: u32, sxt: bool) -> Self {
        Self::Field(UnnamedField { pos, len, sxt })
    }

    fn field_ref(field: FieldDef, len: u32, sxt: bool) -> Self {
        Self::FieldRef(FieldRef { field, len, sxt })
    }
}

#[derive(Clone, Debug)]
pub struct FieldDef {
    name: String,
    func: Option<String>,
    items: Vec<FieldItem>,
}

impl FieldDef {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn func(&self) -> Option<&str> {
        self.func.as_deref()
    }

    pub fn items(&self) -> &[FieldItem] {
        &self.items
    }
}

#[derive(Clone, Debug)]
pub struct ArgDef {
    name: String,
    ty: Option<String>,
}

impl ArgDef {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn ty(&self) -> Option<&str> {
        self.ty.as_deref()
    }
}

#[derive(Clone, Debug)]
pub struct ArgsDef {
    name: String,
    is_extern: bool,
    items: Vec<ArgDef>,
}

impl ArgsDef {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn is_extern(&self) -> bool {
        self.is_extern
    }

    pub fn items(&self) -> &[ArgDef] {
        self.items.as_slice()
    }
}

#[derive(Clone, Debug)]
pub enum Field {
    Field(UnnamedField),
    FieldRef(FieldDef),
}

#[derive(Clone, Debug)]
pub enum ArgsValueKind {
    Const(i64),
    Field(Field),
}

#[derive(Clone, Debug)]
pub struct ArgsValue {
    name: String,
    ty: Option<String>,
    kind: ArgsValueKind,
}

impl ArgsValue {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn ty(&self) -> Option<&str> {
        self.ty.as_deref()
    }

    pub fn kind(&self) -> &ArgsValueKind {
        &self.kind
    }
}

#[derive(Clone, Debug)]
pub enum ValueKind {
    Const(i64),
    Field(Field),
    Args(Vec<ArgsValue>),
}

#[derive(Clone, Debug)]
pub struct Value {
    name: String,
    kind: ValueKind,
}

impl Value {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn kind(&self) -> &ValueKind {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cond {
    name: String,
    invert: bool,
}

impl Cond {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn invert(&self) -> bool {
        self.invert
    }
}

#[derive(Clone, Debug)]
pub struct Pattern<I = DefaultInsn> {
    name: String,
    mask: I,
    opcode: I,
    args: Vec<Value>,
    cond: Vec<Cond>,
}

impl<I> Pattern<I> {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn mask(&self) -> &I {
        &self.mask
    }

    pub fn opcode(&self) -> &I {
        &self.opcode
    }

    pub fn args(&self) -> &[Value] {
        self.args.as_slice()
    }

    pub fn conditions(&self) -> &[Cond] {
        self.cond.as_slice()
    }
}

#[derive(Clone, Debug)]
pub enum OverlapItem<I = DefaultInsn> {
    Pattern(Pattern<I>),
    Group(Box<Group<I>>),
}

#[derive(Clone, Debug, Default)]
pub struct Overlap<I = DefaultInsn> {
    mask: I,
    opcode: I,
    items: Vec<OverlapItem<I>>,
}

impl<I> Overlap<I> {
    pub fn mask(&self) -> &I {
        &self.mask
    }

    pub fn opcode(&self) -> &I {
        &self.opcode
    }

    pub fn iter(&self) -> impl Iterator<Item = &OverlapItem<I>> {
        self.items.iter()
    }
}

#[derive(Clone, Debug)]
pub enum Item<I> {
    Pattern(Pattern<I>),
    Overlap(Box<Overlap<I>>),
}

impl<I: Copy> Item<I> {
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

#[derive(Clone, Debug, Default)]
pub struct Group<I = DefaultInsn> {
    mask: I,
    items: Vec<Item<I>>,
}

impl<I> Group<I> {
    pub fn mask(&self) -> &I {
        &self.mask
    }

    pub fn iter(&self) -> impl Iterator<Item = &Item<I>> {
        self.items.iter()
    }
}

#[derive(Clone, Debug, Default)]
pub struct DecodeTree<I = DefaultInsn> {
    pub fields: HashMap<String, FieldDef>,
    pub args: HashMap<String, ArgsDef>,
    pub root: Group<I>,
}

pub fn parse<I>(src: &str) -> Result<DecodeTree<I>, Errors>
where
    I: Insn,
{
    Parser::<I>::new(src).parse()
}
