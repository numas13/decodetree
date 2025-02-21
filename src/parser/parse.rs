use std::{num::ParseIntError, str};

use nom::{
    branch::alt,
    bytes::complete::{take_while, take_while1},
    character::complete::{
        digit1, hex_digit1, line_ending, multispace0, not_line_ending, one_of, satisfy, space0,
        space1,
    },
    combinator::{consumed, cut, eof, fail, map, opt, recognize, success, value, verify},
    multi::{fold_many0, many0, many0_count, many1, many1_count},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish,
};
use nom_locate::LocatedSpan;

use crate::error::{Error, ErrorKind, Expected};

use super::Cond;

type IResult<'a, T = Span<'a>, I = Span<'a>, E = Error<'a>> = nom::IResult<I, T, E>;

pub type Span<'a> = LocatedSpan<&'a str>;

fn map_err<'a, T, F, M>(mut inner: F, mut map: M) -> impl FnMut(Span<'a>) -> IResult<'a, T>
where
    F: FnMut(Span<'a>) -> IResult<'a, T>,
    M: FnMut(Error<'a>) -> Error<'a>,
{
    use nom::Err;

    move |input| {
        inner(input).map_err(|err| match err {
            Err::Incomplete(n) => Err::Incomplete(n),
            Err::Error(err) => Err::Error(map(err)),
            Err::Failure(err) => Err::Failure(err),
        })
    }
}

fn map_fail<'a, T, F, M>(mut inner: F, mut map: M) -> impl FnMut(Span<'a>) -> IResult<'a, T>
where
    F: FnMut(Span<'a>) -> IResult<'a, T>,
    M: FnMut(Error<'a>) -> Error<'a>,
{
    use nom::Err;

    move |input| {
        inner(input).map_err(|err| match err {
            Err::Incomplete(n) => Err::Incomplete(n),
            Err::Error(err) => Err::Error(map(err)),
            Err::Failure(err) => Err::Failure(map(err)),
        })
    }
}

fn char<'a>(c: char) -> impl FnMut(Span<'a>) -> IResult<'a, char> {
    map_err(nom::character::complete::char(c), move |e| {
        e.or_kind_take(Expected::Char(c), |_| true)
    })
}

fn tag<'a>(s: &'static str) -> impl FnMut(Span<'a>) -> IResult<'a> {
    map_err(nom::bytes::complete::tag(s), move |e| {
        e.or_kind_take(Expected::Tag(s), |c| !c.is_alphanumeric())
    })
}

fn line_break(s: Span) -> IResult<()> {
    value((), delimited(space0, pair(char('\\'), line_ending), space0))(s)
}

fn sp0(s: Span) -> IResult<()> {
    map_err(
        value((), many0_count(alt((line_break, value((), space1))))),
        |e| e.or_kind_take(Expected::Space, |_| true),
    )(s)
}

fn sp1(s: Span) -> IResult<()> {
    map_err(
        value((), many1_count(alt((line_break, value((), space1))))),
        |e| e.or_kind_take(Expected::Space, |_| true),
    )(s)
}

fn line_comment(s: Span) -> IResult<()> {
    value((), preceded(char('#'), not_line_ending))(s)
}

pub fn whitespace0(s: Span) -> IResult<()> {
    value(
        (),
        terminated(
            many0_count(preceded(multispace0, line_comment)),
            multispace0,
        ),
    )(s)
}

fn eol(s: Span) -> IResult<()> {
    let a = pair(sp0, opt(line_comment));
    let b = map_err(alt((line_ending, eof)), |e| {
        e.or_kind_take(ErrorKind::Unexpected, |c| c.is_whitespace())
    });
    value((), preceded(a, b))(s)
}

trait FromStrRadix: Sized {
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError>;
}

macro_rules! impl_from_str_radix {
    ($($ty:ty),+$(,)?) => (
        $(
            impl FromStrRadix for $ty {
                fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError> {
                    <$ty>::from_str_radix(src, radix)
                }
            }
         )+
    );
}

impl_from_str_radix! {
    i8,
    i16,
    i32,
    i64,
    i128,
    u8,
    u16,
    u32,
    u64,
    u128,
}

fn hexadecimal(s: Span) -> IResult<Span> {
    map_fail(
        recognize(preceded(pair(char('0'), one_of("xX")), hex_digit1)),
        move |_| Error::new(s, Expected::Hexadecimal),
    )(s)
}

fn hexadecimal_n<T: FromStrRadix>(s: Span) -> IResult<(Span, T)> {
    let (s, value) = hexadecimal(s)?;
    let n = T::from_str_radix(&s[2..], 16).unwrap();
    Ok((s, (value, n)))
}

fn is_valid_decimal(s: &Span) -> bool {
    if s.len() > 1 {
        s.chars().next().map_or(false, |c| ('1'..='9').contains(&c))
    } else {
        true
    }
}

fn decimal_n<T: FromStrRadix>(s: Span) -> IResult<(Span, T)> {
    let (s, value) = map_fail(
        recognize(pair(opt(char('-')), verify(digit1, is_valid_decimal))),
        move |_| Error::new(s, Expected::Decimal),
    )(s)?;
    Ok((s, (value, T::from_str_radix(*value, 10).unwrap())))
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Number<'a, T> {
    pub value: T,
    pub span: Span<'a>,
}

#[cfg(test)]
impl<'a, T> Number<'a, T> {
    fn new(value: T, span: Span<'a>) -> Self {
        Self { value, span }
    }
}

fn number<T: FromStrRadix>(s: Span) -> IResult<Number<T>> {
    map_err(
        map(alt((decimal_n, hexadecimal_n)), |(span, value)| Number {
            value,
            span,
        }),
        |e| e.or_kind_take(Expected::Number, |c| c.is_whitespace()),
    )(s)
}

fn identifier(s: Span) -> IResult {
    let first = map_err(satisfy(|c: char| c.is_alphabetic() || c == '_'), |e| {
        e.or_kind_take(Expected::Identifier, |c| c.is_whitespace())
    });
    let tail = take_while(|c: char| c.is_alphanumeric() || c == '_');
    recognize(pair(first, tail))(s)
}

fn preceded_name<'a>(prefix: char) -> impl FnMut(Span<'a>) -> IResult<'a> {
    preceded(char(prefix), cut(identifier))
}

fn field_name(s: Span) -> IResult {
    preceded_name('%')(s)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FieldRef<'a> {
    pub name: Span<'a>,
    pub field: Span<'a>,
}

fn field_ref(s: Span) -> IResult<FieldRef> {
    let p = pair(opt(terminated(identifier, char('='))), field_name);
    map(p, |(name, field)| FieldRef {
        name: name.unwrap_or(field),
        field,
    })(s)
}

fn args_name(s: Span) -> IResult {
    preceded_name('&')(s)
}

fn args_ref(s: Span) -> IResult<(Span, Span)> {
    let p = pair(opt(terminated(identifier, char('='))), args_name);
    map(p, |(name, field)| (name.unwrap_or(field), field))(s)
}

fn format_name(s: Span) -> IResult {
    preceded_name('@')(s)
}

fn format_ref(s: Span) -> IResult {
    format_name(s)
}

fn s_len<T: FromStrRadix>(s: Span) -> IResult<(bool, Number<T>)> {
    let (s, signed) = alt((value(true, char('s')), success(false)))(s)?;
    let (s, len) = map_fail(cut(number), |e| {
        if !signed {
            e.or_kind(Expected::NumberOrS)
        } else {
            e
        }
    })(s)?;
    Ok((s, (signed, len)))
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct UnnamedField<'a> {
    pub pos: Number<'a, u32>,
    pub len: Number<'a, u32>,
    pub sxt: bool,
}

impl UnnamedField<'_> {
    pub fn pos(&self) -> u32 {
        self.pos.value
    }

    pub fn len(&self) -> u32 {
        self.len.value
    }

    pub fn sign_extend(&self) -> bool {
        self.sxt
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct NamedField<'a> {
    pub name: Span<'a>,
    pub len: Number<'a, u32>,
    pub sxt: bool,
}

impl<'a> NamedField<'a> {
    fn new(name: Span<'a>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self { name, len, sxt }
    }

    pub fn len(&self) -> u32 {
        self.len.value
    }

    pub fn sign_extend(&self) -> bool {
        self.sxt
    }
}

fn named_field(s: Span) -> IResult<NamedField> {
    map(
        separated_pair(identifier, char(':'), s_len),
        |(name, (sxt, len))| NamedField::new(name, len, sxt),
    )(s)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Field<'a> {
    Unnamed(UnnamedField<'a>),
    Named(NamedField<'a>),
}

impl<'a> Field<'a> {
    fn new_unnamed(pos: Number<'a, u32>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self::Unnamed(UnnamedField { pos, len, sxt })
    }

    fn new_named(name: Span<'a>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self::Named(NamedField { name, len, sxt })
    }
}

fn field(s: Span) -> IResult<Field> {
    let (s, val) = alt((map(number, Ok), map(identifier, Err)))(s)?;
    let (s, (sxt, len)) = preceded(cut(char(':')), s_len)(s)?;
    let ret = match val {
        Ok(num) => Field::new_unnamed(num, len, sxt),
        Err(name) => Field::new_named(name, len, sxt),
    };
    Ok((s, ret))
}

fn field_fn(s: Span) -> IResult<Span> {
    let (s, _) = char('!')(s)?;
    let prefix = pair(tag("function"), char('='));
    cut(preceded(prefix, identifier))(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldDef<'a> {
    pub name: Span<'a>,
    pub func: Option<Span<'a>>,
    pub items: Vec<Field<'a>>,
}

fn field_def(s: Span) -> IResult<FieldDef> {
    let (s, name) = field_name(s)?;
    let (s, items) = many0(preceded(sp1, field))(s)?;
    let (s, func) = opt(preceded(sp1, field_fn))(s)?;
    Ok((s, FieldDef { name, func, items }))
}

fn arg_type(s: Span) -> IResult<Span> {
    map_err(identifier, |e| e.or_kind(Expected::ArgType))(s)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Arg<'a> {
    pub name: Span<'a>,
    pub ty: Option<Span<'a>>,
}

impl<'a> Arg<'a> {
    fn new(name: Span<'a>, ty: Option<Span<'a>>) -> Self {
        Self { name, ty }
    }
}

fn arg(s: Span) -> IResult<Arg> {
    let p = pair(identifier, opt(preceded(char(':'), cut(arg_type))));
    map(p, |(name, ty)| Arg::new(name, ty))(s)
}

fn is_extern(s: Span) -> IResult<bool> {
    let ext = preceded(pair(sp1, char('!')), cut(tag("extern")));
    alt((value(true, ext), success(false)))(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgsDef<'a> {
    pub name: Span<'a>,
    pub is_extern: bool,
    pub args: Vec<Arg<'a>>,
}

impl<'a> ArgsDef<'a> {
    fn new(name: Span<'a>, is_extern: bool, args: Vec<Arg<'a>>) -> Self {
        Self {
            name,
            is_extern,
            args,
        }
    }
}

fn args_def(s: Span) -> IResult<ArgsDef> {
    let (s, name) = args_name(s)?;
    let (s, args) = many0(preceded(sp1, arg))(s)?;
    let (s, is_extern) = is_extern(s)?;
    Ok((s, ArgsDef::new(name, is_extern, args)))
}

fn fixedbits(s: Span) -> IResult {
    take_while1(|c: char| "01.-".contains(c))(s)
}

fn format_field(s: Span) -> IResult<NamedField> {
    named_field(s)
}

fn cond_item(s: Span) -> IResult<Cond> {
    let (s, invert) = alt((value(true, char('~')), success(false)))(s)?;
    let (s, name) = map_err(identifier, |e| {
        if !invert {
            e.or_kind(Expected::IdentifierOrTilde)
        } else {
            e
        }
    })(s)?;
    Ok((s, Cond { invert, name }))
}

fn cond_list(s: Span) -> IResult<Vec<Cond>> {
    if let (s, Some(_)) = opt(char('?'))(s)? {
        if let (s, Some(first)) = preceded(sp0, opt(cond_item))(s)? {
            fold_many0(
                preceded(sp1, cut(cond_item)),
                || vec![first.clone()],
                |mut vec, cond| {
                    vec.push(cond);
                    vec
                },
            )(s)
        } else {
            Ok((s, vec![]))
        }
    } else {
        Ok((s, vec![]))
    }
}

pub type FormatItem<'a> = PatternItem<'a>;

fn format_item(s: Span) -> IResult<FormatItem> {
    let p = alt((
        map(fixedbits, FormatItem::FixedBits),
        map(format_field, FormatItem::FixedField),
        map(field_ref, FormatItem::FieldRef),
        map(args_ref, |(n, s)| FormatItem::ArgsRef(n, s)),
        map(const_value, FormatItem::Const),
    ));
    map_err(p, |e| {
        e.or_kind(Expected::Msg("fixedbit, field, field_ref or args_ref"))
    })(s)
}

pub type FormatDef<'a> = PatternDef<'a>;

fn format_def(s: Span) -> IResult<FormatDef> {
    let parser = tuple((
        format_name,
        cut(many1(preceded(sp1, format_item))),
        preceded(sp0, cond_list),
    ));
    let (s, (raw, (name, items, cond))) = consumed(parser)(s)?;
    Ok((s, FormatDef::new(raw, name, items, cond)))
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Const<'a> {
    pub name: Span<'a>,
    pub num: Number<'a, i64>,
}

impl Const<'_> {
    pub fn value(&self) -> i64 {
        self.num.value
    }
}

fn const_value(s: Span) -> IResult<Const> {
    map(
        separated_pair(identifier, cut(char('=')), cut(number)),
        |(name, num)| Const { name, num },
    )(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternItem<'a> {
    ArgsRef(Span<'a>, Span<'a>),
    FormatRef(Span<'a>),
    FixedBits(Span<'a>),
    FixedField(NamedField<'a>),
    FieldRef(FieldRef<'a>),
    Const(Const<'a>),
}

#[cfg(test)]
impl<'a> PatternItem<'a> {
    fn fixed_bits(value: Span<'a>) -> Self {
        Self::FixedBits(value)
    }

    fn format_ref(name: Span<'a>) -> Self {
        Self::FormatRef(name)
    }

    fn args_ref(name: Span<'a>) -> Self {
        Self::ArgsRef(name, name)
    }

    fn field(name: Span<'a>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self::FixedField(NamedField { name, len, sxt })
    }

    fn field_ref(name: Span<'a>, field: Span<'a>) -> Self {
        Self::FieldRef(FieldRef { name, field })
    }

    fn const_val(name: Span<'a>, num: Number<'a, i64>) -> Self {
        Self::Const(Const { name, num })
    }
}

fn pattern_item(s: Span) -> IResult<PatternItem> {
    let p = alt((
        map(args_ref, |(n, s)| PatternItem::ArgsRef(n, s)),
        map(format_ref, PatternItem::FormatRef),
        map(fixedbits, PatternItem::FixedBits),
        map(format_field, PatternItem::FixedField),
        map(field_ref, PatternItem::FieldRef),
        map(const_value, PatternItem::Const),
    ));
    map_err(p, |e| {
        e.or_kind(Expected::Msg(
            "fixedbit, field, field_ref, args_ref, format_ref or const_value",
        ))
    })(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternDef<'a> {
    pub raw: Span<'a>,
    pub name: Span<'a>,
    pub items: Vec<PatternItem<'a>>,
    pub cond: Vec<Cond<'a>>,
}

impl<'a> PatternDef<'a> {
    fn new(
        raw: Span<'a>,
        name: Span<'a>,
        items: Vec<PatternItem<'a>>,
        cond: Vec<Cond<'a>>,
    ) -> Self {
        Self {
            raw,
            name,
            items,
            cond,
        }
    }
}

fn pattern_def(s: Span) -> IResult<PatternDef> {
    let parser = tuple((
        identifier,
        cut(many1(preceded(sp1, pattern_item))),
        preceded(sp0, cond_list),
    ));
    let (s, (raw, (name, items, cond))) = consumed(parser)(s)?;
    Ok((s, PatternDef::new(raw, name, items, cond)))
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GroupItem<'a> {
    PatternDef(PatternDef<'a>),
    Group(Box<Group<'a>>),
}

fn group_item(s: Span) -> IResult<GroupItem> {
    delimited(
        whitespace0,
        alt((
            map(pattern_def, GroupItem::PatternDef),
            map(group, |g| GroupItem::Group(Box::new(g))),
        )),
        eol,
    )(s)
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Group<'a> {
    pub overlap: bool,
    pub items: Vec<GroupItem<'a>>,
}

impl<'a> Group<'a> {
    fn push_item(&mut self, item: GroupItem<'a>) {
        match item {
            GroupItem::PatternDef(..) => self.items.push(item),
            GroupItem::Group(group) => self.push_group(group),
        }
    }

    pub fn push_group(&mut self, mut other: Box<Group<'a>>) {
        if other.items.is_empty() {
            return;
        }

        if other.items.len() == 1 {
            let item = other.items.remove(0);
            match item {
                GroupItem::PatternDef(..) => self.items.push(item),
                // NOTE: Pattern will travel up to the top if parent groups
                // does not have any other patterns.
                GroupItem::Group(..) => unreachable!(),
            }
        } else if self.overlap == other.overlap {
            self.items.append(&mut other.items);
        } else {
            self.items.push(GroupItem::Group(other));
        }
    }
}

fn group_delimited<'a>(
    open: char,
    close: char,
    overlap: bool,
) -> impl FnMut(Span<'a>) -> IResult<'a, Group<'a>> {
    delimited(
        preceded(whitespace0, char(open)),
        cut(fold_many0(
            group_item,
            move || Group {
                overlap,
                items: vec![],
            },
            |mut group, item| {
                group.push_item(item);
                group
            },
        )),
        preceded(whitespace0, cut(char(close))),
    )
}

fn group(s: Span) -> IResult<Group> {
    alt((
        group_delimited('{', '}', true),
        group_delimited('[', ']', false),
    ))(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt<'a> {
    FieldDef(FieldDef<'a>),
    ArgsDef(ArgsDef<'a>),
    FormatDef(FormatDef<'a>),
    PatternDef(PatternDef<'a>),
    Group(Group<'a>),
}

pub fn stmt(s: Span) -> Result<Option<(Span, Stmt)>, Error> {
    let (s, _) = whitespace0(s).unwrap();
    if s.is_empty() {
        return Ok(None);
    }
    let p = alt((
        map(group, Stmt::Group),
        map(field_def, Stmt::FieldDef),
        map(args_def, Stmt::ArgsDef),
        map(format_def, Stmt::FormatDef),
        map(pattern_def, Stmt::PatternDef),
        map_err(fail, |e| {
            e.or_kind_take(ErrorKind::Unexpected, |c| c.is_whitespace())
        }),
    ));
    terminated(p, eol)(s).finish().map(Some)
}

#[cfg(test)]
#[rustfmt::skip]
#[allow(clippy::int_plus_one)]
mod tests {
    use super::*;

    /// Helper macro to define a span within the slice
    macro_rules! span {
        ($src:ident[$offset:expr; $len:expr]) => ({
            assert!($offset + $len <= $src.len());
            let line = $src.lines().count() as u32;
            unsafe { Span::new_from_raw_offset($offset, line, &$src[$offset..$offset + $len], ()) }
        });
    }

    /// Helper macro to define a span within the slice and parse it as a number
    macro_rules! num {
        ($src:ident[$offset:expr; $len:expr]) => ({
            let s = span!($src[$offset; $len]);
            Number::new(s.parse().unwrap(), s)
        });
    }

    #[test]
    fn parse_field_def() {
        let s = "%name 10:5 20:s12 !function=func";
        assert_eq!(Ok((
            span!(s[32; 0]),
            FieldDef {
                name: span!(s[1; 4]),
                func: Some(span!(s[28; 4])),
                items: vec![
                    Field::new_unnamed(num!(s[6; 2]), num!(s[9; 1]), false),
                    Field::new_unnamed(num!(s[11; 2]), num!(s[15; 2]), true),
                ],
            },
        )), field_def(s.into()));
    }

    #[test]
    fn parse_args_def() {
        let s = "&name foo foo:bar !extern";
        assert_eq!(Ok((
            span!(s[25; 0]),
            ArgsDef {
                name: span!(s[1; 4]),
                is_extern: true,
                args: vec![
                    Arg::new(span!(s[6; 3]), None),
                    Arg::new(span!(s[10; 3]), Some(span!(s[14; 3]))),
                ],
            }
        )), args_def(s.into()));
    }

    #[test]
    fn parse_format_def() {
        let s = "@name 01.- a:s4 %b c=%d &args f=13";
        assert_eq!(Ok((
            span!(s[34; 0]),
            FormatDef {
                raw: span!(s[0; 34]),
                name: span!(s[1; 4]),
                items: vec![
                    FormatItem::fixed_bits(span!(s[6; 4])),
                    FormatItem::field(span!(s[11; 1]), num!(s[14; 1]), true),
                    FormatItem::field_ref(span!(s[17; 1]), span!(s[17; 1])),
                    FormatItem::field_ref(span!(s[19; 1]), span!(s[22; 1])),
                    FormatItem::args_ref(span!(s[25; 4])),
                    FormatItem::const_val(span!(s[30; 1]), num!(s[32; 2])),
                ],
                cond: vec![],
            }
        )), format_def(s.into()));
    }

    #[test]
    fn parse_pattern_def() {
        let s = "name 01.- a:s4 %b c=%d &args @e f=13";
        assert_eq!(Ok((
            span!(s[36; 0]),
            PatternDef {
                raw: span!(s[0; 36]),
                name: span!(s[0; 4]),
                items: vec![
                    PatternItem::fixed_bits(span!(s[5; 4])),
                    PatternItem::field(span!(s[10; 1]), num!(s[13; 1]), true),
                    PatternItem::field_ref(span!(s[16; 1]), span!(s[16; 1])),
                    PatternItem::field_ref(span!(s[18; 1]), span!(s[21; 1])),
                    PatternItem::args_ref(span!(s[24; 4])),
                    PatternItem::format_ref(span!(s[30; 1])),
                    PatternItem::const_val(span!(s[32; 1]), num!(s[34; 2])),
                ],
                cond: vec![],
            }
        )), pattern_def(s.into()));
    }
}
