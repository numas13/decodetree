mod error;
mod parser;

#[cfg(feature = "gen")]
pub mod gen;

use std::{collections::HashMap, fmt::LowerHex, hash::Hash, mem};

pub use crate::error::{ErrorPrinter, Errors};
pub use crate::parser::{
    Args, Field, FieldItem, Group, Item, Overlap, OverlapItem, Parser, Pattern, ValueKind,
};

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

#[derive(Clone, Debug, Default)]
pub struct DecodeTree<I = DefaultInsn> {
    pub fields: HashMap<String, Field>,
    pub args: HashMap<String, Args>,
    pub root: Group<I>,
}

pub fn parse<I>(src: &str) -> Result<DecodeTree<I>, Errors>
where
    I: Insn,
{
    Parser::new(src).parse()
}
