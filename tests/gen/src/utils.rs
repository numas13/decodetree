#![allow(dead_code)]

use std::mem;

pub trait ZExtract<U>: Sized {
    fn zextract(&self, pos: u32, len: u32) -> U;
}

pub trait SExtract<S>: Sized {
    fn sextract(&self, pos: u32, len: u32) -> S;
}

macro_rules! impl_extract {
    ($($uint:ty = $sint:ty),+ $(,)?) => (
        $(
            impl ZExtract<$uint> for $uint {
                fn zextract(&self, pos: u32, len: u32) -> $uint {
                    let w = mem::size_of::<$uint>() as u32 * 8;
                    (*self as $uint << (w - pos - len)) >> (w - len)
                }
            }

            impl SExtract<$sint> for $uint {
                fn sextract(&self, pos: u32, len: u32) -> $sint {
                    let w = mem::size_of::<$uint>() as u32 * 8;
                    (*self as $uint << (w - pos - len)) as $sint >> (w - len)
                }
            }
        )+
    );
}

impl_extract! {
    u8 = i8,
    u16 = i16,
    u32 = i32,
    u64 = i64,
    u128 = i128,
}

pub fn zextract<U, T: ZExtract<U>>(value: T, pos: u32, len: u32) -> U {
    value.zextract(pos, len)
}

pub fn sextract<S, T: SExtract<S>>(value: T, pos: u32, len: u32) -> S {
    value.sextract(pos, len)
}

pub trait Deposit: Sized {
    fn deposit<F: Into<Self>>(&self, pos: u32, len: u32, field: F) -> Self;
}

macro_rules! impl_deposit {
    ($($uint:ty = $sint:ty),+ $(,)?) => {
        $(
            impl Deposit for $uint {
                fn deposit<F: Into<Self>>(&self, pos: u32, len: u32, field: F) -> Self {
                    let mask = (1 as $uint << len).wrapping_sub(1) << pos;
                    (*self & !mask) | ((field.into() << pos) & mask)
                }
            }
         )+
    };
}

impl_deposit! {
    u8 = i8,
    u16 = i16,
    u32 = i32,
    u64 = i64,
    u128 = i128,
}

pub fn deposit<T: Deposit, F: Into<T>>(value: T, pos: u32, len: u32, field: F) -> T {
    value.deposit(pos, len, field)
}
