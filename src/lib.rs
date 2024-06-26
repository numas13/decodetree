//! # Decodetree parser and generator.
//!
//! This crate provides utilities to generate decoding tries.
//!
//! To be able to decode instructions you need their descriptions writteng in special format.
//! The syntax can be found in [specification][1].
//!
//! [1]: https://www.qemu.org/docs/master/devel/decodetree.html

mod error;
mod parser;

#[cfg(feature = "gen")]
pub mod gen;

use std::{collections::HashMap, fmt::LowerHex, hash::Hash, mem, ops::Deref, rc::Rc};

use crate::parser::Span;

pub use crate::error::Errors;
pub use crate::parser::Parser;

#[cfg(feature = "gen")]
pub use crate::gen::Generator;

type DefaultInsn = u32;

/// Default type for storing strings.
pub type Str = Box<str>;

/// Helper trait to work with instruction bits.
pub trait Insn: Sized + Copy + Clone + Eq + Ord + Hash + LowerHex + Default {
    /// Returns the size in bits of the instruction word.
    fn width() -> u32;

    /// Returns value with all bits set to `0`.
    fn zero() -> Self;

    /// Returns value with all bits set to `1`.
    fn ones() -> Self;

    /// Set bit at `offset` to `bit`.
    fn set_bit(&mut self, offset: u32, bit: bool);

    /// The bitwise NOT operation.
    fn bit_not(&self) -> Self;

    /// The bitwise AND operation.
    fn bit_and(&self, other: &Self) -> Self;

    /// The bitwise AND operation with inversed `other`.
    fn bit_andn(&self, other: &Self) -> Self;

    /// The bitwise OR operation.
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

/// An unnamed field with position and length.
#[derive(Copy, Clone, Debug)]
pub struct UnnamedField {
    pos: u32,
    len: u32,
    sxt: bool,
}

impl UnnamedField {
    /// Returns the bit position of this field.
    pub fn pos(&self) -> u32 {
        self.pos
    }

    /// Returns the length of this field.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u32 {
        self.len
    }

    /// Returns `true` if this field must be extended with sign.
    pub fn sxt(&self) -> bool {
        self.sxt
    }
}

/// A reference to a field definition.
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

    /// Returns the field definition for this reference.
    pub fn field(&self) -> &FieldDef<S> {
        &self.field
    }

    /// Returns the length of this field reference.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u32 {
        self.len
    }

    /// Returns `true` if this field must be extended with sign.
    pub fn sxt(&self) -> bool {
        self.sxt
    }
}

/// A list of subfield types for field definition.
#[derive(Clone, Debug)]
pub enum FieldItem<S = Str> {
    /// Unnamed subfield with position and length.
    Field(UnnamedField),
    /// Reference to another field definition.
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

/// A field definition.
#[derive(Clone, Debug)]
pub struct FieldDef<S = Str> {
    name: S,
    func: Option<S>,
    items: Vec<FieldItem<S>>,
}

impl<S> FieldDef<S> {
    /// Returns the name for this field.
    pub fn name(&self) -> &S {
        &self.name
    }

    /// Returns an optional user defined function for this field.
    pub fn func(&self) -> Option<&S> {
        self.func.as_ref()
    }

    /// Returns a slice containing all subfields for this field.
    pub fn items(&self) -> &[FieldItem<S>] {
        &self.items
    }

    /// Returns an iterator over all subfields for this field.
    pub fn iter(&self) -> impl Iterator<Item = &FieldItem<S>> {
        self.items.iter()
    }
}

/// A value definition for a set.
#[derive(Clone, Debug)]
pub struct SetValueDef<S = Str> {
    name: S,
    ty: Option<S>,
}

impl<S> SetValueDef<S> {
    /// Returns the name for this value.
    pub fn name(&self) -> &S {
        &self.name
    }

    /// Returns the type for this value if specified.
    pub fn ty(&self) -> Option<&S> {
        self.ty.as_ref()
    }
}

/// A definition for an argument set.
#[derive(Clone, Debug)]
pub struct SetDef<S = Str> {
    name: S,
    is_extern: bool,
    items: Vec<SetValueDef<S>>,
}

impl<S> SetDef<S> {
    /// Returns the name for this definition.
    pub fn name(&self) -> &S {
        &self.name
    }

    /// Returns `true` if this definition is marked as defined by user.
    pub fn is_extern(&self) -> bool {
        self.is_extern
    }

    /// Returns a slice containing all values for this set definition.
    pub fn values(&self) -> &[SetValueDef<S>] {
        self.items.as_slice()
    }

    /// Returns an iterator over values for this set definition.
    pub fn iter(&self) -> impl Iterator<Item = &SetValueDef<S>> {
        self.items.iter()
    }
}

/// A list of field types.
#[derive(Clone, Debug)]
pub enum Field<S = Str> {
    /// An unnamed field with position and length.
    Field(UnnamedField),
    /// A refenrence to a field definition.
    FieldRef(Rc<FieldDef<S>>),
}

/// A list of set value types.
#[derive(Clone, Debug)]
pub enum SetValueKind<S = Str> {
    /// A constant value.
    Const(i64),
    /// A field value.
    Field(Field<S>),
}

/// A value for a set.
#[derive(Clone, Debug)]
pub struct SetValue<S = Str> {
    name: S,
    ty: Option<S>,
    kind: Option<SetValueKind<S>>,
}

impl<S> SetValue<S> {
    /// Returns the name for this value.
    pub fn name(&self) -> &S {
        &self.name
    }

    /// Returns the type for this value if specified.
    pub fn ty(&self) -> Option<&S> {
        self.ty.as_ref()
    }

    /// Returns the corresponding [`SetValueKind`] for this set value.
    pub fn kind(&self) -> &SetValueKind<S> {
        self.kind.as_ref().expect("handled by parser")
    }
}

/// A list of value types.
#[derive(Clone, Debug)]
pub enum ValueKind<S = Str> {
    /// A constant value.
    Const(i64),
    /// A field value.
    Field(Field<S>),
    /// A set of values.
    Set(Vec<SetValue<S>>),
}

/// A value for instruction pattern.
#[derive(Clone, Debug)]
pub struct Value<S = Str> {
    name: S,
    kind: ValueKind<S>,
}

impl<S> Value<S> {
    /// Returns the name for this value.
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

    fn new_set(name: S, args: Vec<SetValue<S>>) -> Self {
        Self {
            name,
            kind: ValueKind::Set(args),
        }
    }

    /// Returns the corresponding [`ValueKind`] for this value.
    pub fn kind(&self) -> &ValueKind<S> {
        &self.kind
    }

    /// Returns `true` if this value contains [`ValueKind::Set`].
    pub fn is_set(&self) -> bool {
        matches!(self.kind, ValueKind::Set(..))
    }

    /// Returns `true` if this value contains [`ValueKind::Field`].
    pub fn is_field(&self) -> bool {
        matches!(self.kind, ValueKind::Field(..))
    }

    /// Returns `true` if this value contains [`ValueKind::Const`].
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

/// A condition for an instruction pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cond<S = Str> {
    name: S,
    invert: bool,
}

impl<S> Cond<S> {
    /// Returns a name of this condition.
    pub fn name(&self) -> &S {
        &self.name
    }

    /// Returns `true` if the condition is inverted.
    pub fn invert(&self) -> bool {
        self.invert
    }
}

/// An instruction pattern.
#[derive(Clone, Debug)]
pub struct Pattern<I = DefaultInsn, S = Str> {
    #[cfg(feature = "raw")]
    raw: S,
    name: S,
    mask: I,
    opcode: I,
    size: u32,
    args: Vec<Value<S>>,
    cond: Vec<Cond<S>>,
}

impl<I, S> Pattern<I, S> {
    /// Raw pattern string from source code.
    #[cfg(feature = "raw")]
    pub fn raw(&self) -> &S {
        &self.raw
    }

    /// Pattern name.
    pub fn name(&self) -> &S {
        &self.name
    }

    /// Mask for instruction to match with `opcode`.
    pub fn mask(&self) -> &I {
        &self.mask
    }

    /// Opcode to match with masked instruction.
    pub fn opcode(&self) -> &I {
        &self.opcode
    }

    /// Pattern size in bits.
    #[allow(clippy::len_without_is_empty)]
    pub fn size(&self) -> u32 {
        self.size
    }

    /// Returns a slice containing all values for this pattern.
    pub fn values(&self) -> &[Value<S>] {
        self.args.as_slice()
    }

    /// Check if this pattern has any conditions.
    pub fn has_conditions(&self) -> bool {
        !self.conditions().is_empty()
    }

    /// Returns a slice containing all conditions for this pattern.
    pub fn conditions(&self) -> &[Cond<S>] {
        self.cond.as_slice()
    }
}

impl<'a, I> Pattern<I, Span<'a>> {
    fn push_args(&mut self, value: Value<Span<'a>>) {
        fn convert(kind: ValueKind<Span>) -> SetValueKind<Span> {
            match kind {
                ValueKind::Const(value) => SetValueKind::Const(value),
                ValueKind::Field(field) => SetValueKind::Field(field),
                ValueKind::Set(..) => panic!("nested args set"),
            }
        }

        match &value.kind {
            ValueKind::Set(..) => self.args.push(value),
            ValueKind::Field(..) | ValueKind::Const(..) => {
                // fill empty slot
                for arg in self.args.iter_mut().rev() {
                    if let ValueKind::Set(args) = &mut arg.kind {
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
                    if let ValueKind::Set(args) = &mut arg.kind {
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
    fn push_condition(&mut self, cond: Cond<S>) {
        if let Some(prev) = self.cond.iter_mut().find(|i| i.name == cond.name) {
            prev.invert = cond.invert;
        } else {
            self.cond.push(cond.clone());
        }
    }
}

/// A child item for a overlap group.
#[derive(Clone, Debug)]
pub enum OverlapItem<I = DefaultInsn, S = Str> {
    /// An instruction pattern.
    Pattern(Pattern<I, S>),
    /// A non-overlap group.
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

/// A group for overlapping patterns.
#[derive(Clone, Debug, Default)]
pub struct Overlap<I = DefaultInsn, S = Str> {
    mask: I,
    opcode: I,
    items: Vec<OverlapItem<I, S>>,
}

impl<I, S> Overlap<I, S> {
    /// Mask for instruction to match with `opcode`.
    pub fn mask(&self) -> &I {
        &self.mask
    }

    /// Opcode to match with masked instruction.
    pub fn opcode(&self) -> &I {
        &self.opcode
    }

    /// Returns a slice containing all childs of the overlap group.
    pub fn as_slice(&self) -> &[OverlapItem<I, S>] {
        self.items.as_slice()
    }

    /// Returns an iterator over this overlap group.
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

impl<I: Insn, S> Overlap<I, S> {
    fn optimize(&mut self) {
        for item in &mut self.items {
            match item {
                OverlapItem::Pattern(_) => {}
                OverlapItem::Group(group) => {
                    group.optimize();
                }
            }
        }
    }
}

/// A child item for a non-overlap group.
#[derive(Clone, Debug)]
pub enum Item<I, S = Str> {
    /// An instruction pattern.
    Pattern(Pattern<I, S>),
    /// An overlap group.
    Overlap(Box<Overlap<I, S>>),
    /// A non-overlap group.
    Group(Box<Group<I, S>>),
}

impl<I, S> Item<I, S> {
    /// Mask for instruction to match with `opcode`.
    pub fn mask(&self) -> &I {
        match self {
            Self::Pattern(i) => &i.mask,
            Self::Overlap(i) => &i.mask,
            Self::Group(i) => &i.mask,
        }
    }

    /// Opcode to match with masked instruction.
    pub fn opcode(&self) -> &I {
        match self {
            Self::Pattern(i) => &i.opcode,
            Self::Overlap(i) => &i.opcode,
            Self::Group(i) => &i.opcode,
        }
    }

    fn first_pattern(&self) -> &Pattern<I, S> {
        match self {
            Self::Pattern(pattern) => pattern,
            Self::Overlap(overlap) => overlap.first_pattern(),
            Self::Group(group) => group.first_pattern(),
        }
    }
}

/// A group for non-overlapping patterns.
#[derive(Clone, Debug, Default)]
pub struct Group<I = DefaultInsn, S = Str> {
    mask: I,
    opcode: I,
    items: Vec<Item<I, S>>,
}

impl<I, S> Group<I, S> {
    /// Mask for instruction to match with `opcode`.
    pub fn mask(&self) -> &I {
        &self.mask
    }

    /// Opcode to match with masked instruction.
    pub fn opcode(&self) -> &I {
        &self.opcode
    }

    /// Returns a slice containing all childs of the group.
    pub fn as_slice(&self) -> &[Item<I, S>] {
        self.items.as_slice()
    }

    /// Returns an iterator over this non-overlap group.
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

impl<I: Insn, S> Group<I, S> {
    /// Returns shared bits for all child items.
    pub fn shared_mask(&self) -> I {
        self.items
            .iter()
            .fold(I::ones(), |mask, i| mask.bit_and(i.mask()))
    }

    fn optimize(&mut self) {
        let shared_mask = self.shared_mask();

        let mut map = HashMap::<_, Vec<_>>::new();
        for i in mem::take(&mut self.items) {
            map.entry(i.opcode().bit_and(&shared_mask))
                .or_default()
                .push(i);
        }

        let mut items: Vec<_> = map.into_iter().collect();
        items.sort_by(|a, b| a.0.cmp(&b.0));

        for (opcode, mut items) in items.into_iter() {
            let item = if items.len() == 1 {
                let mut item = items.remove(0);
                match &mut item {
                    Item::Pattern(..) => {}
                    Item::Overlap(overlap) => {
                        overlap.optimize();
                    }
                    Item::Group(..) => {
                        unreachable!("parser must flatten the tree");
                    }
                }
                item
            } else {
                let mut group = Group {
                    mask: shared_mask,
                    opcode,
                    items,
                };
                group.optimize();
                Item::Group(Box::new(group))
            };
            self.items.push(item);
        }
    }
}

/// Container for field definitions, set definitions and patterns.
#[derive(Clone, Debug, Default)]
pub struct DecodeTree<I = DefaultInsn, S = Str> {
    /// Field definitions.
    pub fields: Vec<Rc<FieldDef<S>>>,
    /// Set definitions.
    pub args: Vec<SetDef<S>>,
    /// The root non-overlap group for this decodetree.
    pub root: Group<I, S>,
}

impl<I: Insn, S> DecodeTree<I, S> {
    /// Optimize this tree.
    ///
    /// Common parts of patterns will be found and placed into separate non-overlap groups.
    ///
    /// # Examples
    ///
    /// ```text
    /// a       .... .... .... ..00
    ///
    /// b_a     .... .... ..00 ..01
    /// b_b     .... .... ..01 ..01
    /// b_c     .... .... ..10 ..01
    ///
    /// b_d_a   0... .... ..11 ..01
    /// b_d_b   1... .... ..11 ..01
    ///
    /// c       .... .... .... ..10
    /// d       .... .... .... ..11
    /// ```
    ///
    /// Without optimization:
    ///
    /// ```text
    /// [ # 0000:0003
    ///   0000:0003:16 a
    ///   0001:0033:16 b_a
    ///   0011:0033:16 b_b
    ///   0021:0033:16 b_c
    ///   0031:8033:16 b_d_a
    ///   8031:8033:16 b_d_b
    ///   0002:0003:16 c
    ///   0003:0003:16 d
    /// ]
    /// ```
    ///
    /// With optimization:
    ///
    /// ```text
    /// [ # 0000:0003
    ///   0000:0003:16 a
    ///   [ # 0001:0003
    ///     0001:0033:16 b_a
    ///     0011:0033:16 b_b
    ///     0021:0033:16 b_c
    ///     [ # 0031:0033
    ///       0031:8033:16 b_d_a
    ///       8031:8033:16 b_d_b
    ///     ]
    ///   ]
    ///   0002:0003:16 c
    ///   0003:0003:16 d
    /// ]
    /// ```
    pub fn optimize(&mut self) {
        self.root.optimize();
    }
}

/// Parse a decodetree from a string.
///
/// Use [`Parser::new`] to override default parameters.
///
/// # Examples
///
/// ```rust
/// # use decodetree::{Item, ValueKind};
/// let src = r#"
///     ## Fields:
///     %rd     4:4
///     %rs     8:4
///     ## Argument sets:
///     &r      rd rs
///     ## Formats:
///     @r      .... .... .... .... &r %rd %rs
///     ## Patterns:
///     add     0000 .... .... 0000 @r
///     sub     1000 .... .... 0000 @r
/// "#;
///
/// let tree = decodetree::from_str::<u16, &str>(&src).unwrap();
///
/// for (i, item) in tree.root.iter().enumerate() {
///     let Item::Pattern(p) = item else { panic!() };
///     assert_eq!(p.name(), &["add", "sub"][i]);
///
///     let args = p.values();
///     assert!(args[0].is_set());
///     assert_eq!(args[0].name(), &"r");
///
///     let ValueKind::Set(set) = args[0].kind() else { panic!() };
///     assert_eq!(set[0].name(), &"rd");
///     assert_eq!(set[1].name(), &"rs");
/// }
/// ```
pub fn from_str<'src, I, S>(src: &'src str) -> Result<DecodeTree<I, S>, Errors>
where
    I: Insn,
    S: Ord + From<&'src str>,
{
    Parser::<I, S>::new(src).parse()
}
