use std::{
    collections::hash_map::{Entry, HashMap},
    fmt,
    num::ParseIntError,
    str,
};

use nom::{
    branch::alt,
    bytes::complete::{take_while, take_while1},
    character::complete::{
        digit1, hex_digit1, line_ending, multispace0, not_line_ending, one_of, satisfy, space0,
        space1,
    },
    combinator::{cut, eof, map, opt, recognize, success, value, verify},
    error::{ErrorKind as NomErrorKind, FromExternalError, ParseError},
    multi::{many0, many0_count, many1, many1_count},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    Finish, InputTake,
};
use nom_locate::LocatedSpan;

type IResult<'a, T = Span<'a>, I = Span<'a>, E = Error<'a>> = nom::IResult<I, T, E>;
type Span<'a> = LocatedSpan<&'a str>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Expected {
    Identifier,
    Hexadecimal,
    Decimal,
    Number,
    NumberOrIdentifier,
    NumberOrS,
    Space,
    ArgType,
    Char(char),
    Tag(&'static str),
    Msg(&'static str),
}

impl fmt::Display for Expected {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier => write!(fmt, "identifier"),
            Self::Hexadecimal => write!(fmt, "hexadecimal number"),
            Self::Decimal => write!(fmt, "decimal number"),
            Self::Number => write!(fmt, "number"),
            Self::NumberOrS => write!(fmt, "number or `s`"),
            Self::NumberOrIdentifier => write!(fmt, "number or identifier"),
            Self::Space => write!(fmt, "space"),
            Self::ArgType => write!(fmt, "arg type"),
            Self::Char(c) => write!(fmt, "char `{c}`"),
            Self::Tag(s) => write!(fmt, "`{s}`"),
            Self::Msg(s) => write!(fmt, "{s}"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Token {
    Field,
    Args,
    Format,
}

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Field => write!(fmt, "field"),
            Self::Args => write!(fmt, "argument set"),
            Self::Format => write!(fmt, "format"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
enum ErrorKind<'a> {
    Expected(Expected),
    Unexpected,
    FieldLenZero,
    FieldPos(u32, u32),
    Redefined(Token, Span<'a>),
    Undefined(Token),
    Nom(NomErrorKind),
}

impl From<Expected> for ErrorKind<'_> {
    fn from(value: Expected) -> Self {
        Self::Expected(value)
    }
}

impl From<NomErrorKind> for ErrorKind<'_> {
    fn from(value: NomErrorKind) -> Self {
        Self::Nom(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Error<'a> {
    input: Span<'a>,
    kind: ErrorKind<'a>,
}

impl<'a> Error<'a> {
    fn new<K: Into<ErrorKind<'a>>>(input: Span<'a>, kind: K) -> Self {
        Self {
            input,
            kind: kind.into(),
        }
    }

    fn or_kind<K: Into<ErrorKind<'a>>>(self, kind: K) -> Self {
        Self {
            kind: kind.into(),
            ..self
        }
    }

    fn printer<'b: 'a>(&'b self, file: &'a str, src: &'a str) -> ErrorPrinter<'b> {
        ErrorPrinter {
            err: self,
            file,
            src,
        }
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use ErrorKind as E;

        match self.kind {
            E::Expected(i) => write!(fmt, "expected {i}"),
            E::Unexpected => write!(fmt, "unexpected"),
            E::Redefined(token, prev) => {
                write!(fmt, "the {} `{}` is defined multiple times", token, *prev)
            }
            E::Undefined(token) => write!(fmt, "cannot find {} `{}`", token, *self.input),
            E::Nom(i) => write!(fmt, "{i:?}"),
            E::FieldLenZero => write!(fmt, "field length is zero"),
            E::FieldPos(_, _) => write!(fmt, "field offset is outside the width of instruction"),
        }
    }
}

impl<'a> ParseError<Span<'a>> for Error<'a> {
    fn from_error_kind(input: Span<'a>, kind: NomErrorKind) -> Self {
        Self {
            input,
            kind: ErrorKind::Nom(kind),
        }
    }

    fn append(_input: Span<'a>, _kind: NomErrorKind, other: Self) -> Self {
        other
    }
}

impl<'a, E> FromExternalError<Span<'a>, E> for Error<'a> {
    fn from_external_error(input: Span<'a>, kind: NomErrorKind, _e: E) -> Self {
        Error::new(input, kind)
    }
}

pub struct ErrorPrinter<'a> {
    err: &'a Error<'a>,
    file: &'a str,
    src: &'a str,
}

impl<'a> ErrorPrinter<'a> {
    fn location_line_width(&self, mut ln: u32) -> usize {
        for i in 1.. {
            if ln < 10 {
                return i;
            }
            ln /= 10;
        }
        unreachable!();
    }

    fn get_span_line(&self, span: Span<'a>) -> &'a str {
        let start = span.location_offset() - (span.get_column() - 1);
        let line = &self.src[start..];
        let end = line
            .char_indices()
            .find(|(_, c)| *c == '\n')
            .map(|(i, _)| i)
            .unwrap_or(line.len());
        &line[..end]
    }

    fn get_span_prev_line(&self, span: Span<'a>) -> &'a str {
        let end = span.location_offset() - (span.get_column() - 1);
        let start = self.src[..end]
            .char_indices()
            .rev()
            .find(|(_, c)| *c == '\n')
            .map(|(i, _)| i)
            .unwrap_or(0);
        &self.src[start..end]
    }

    fn get_err_line(&self) -> &'a str {
        self.get_span_line(self.err.input)
    }
}

impl fmt::Display for ErrorPrinter<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "error: {}", self.err)?;
        let ln = self.err.input.location_line();
        let lw = self.location_line_width(ln);
        let col = self.err.input.get_utf8_column() - 1;
        let width = self.err.input.chars().count();
        writeln!(fmt, "{:<lw$}--> {}:{ln}:{}", ' ', self.file, col + 1)?;

        match self.err.kind {
            ErrorKind::Redefined(token, prev) => {
                let lp = prev.location_line();
                writeln!(fmt, "{:<lw$} | {}", lp, self.get_span_line(prev))?;
                let col = prev.get_utf8_column() - 1;
                writeln!(
                    fmt,
                    "{:<lw$} | {:col$}{:-<width$} previous definition of the {} `{}` here",
                    ' ', "", '-', token, *prev
                )?;
                match ln.abs_diff(lp) {
                    0 | 1 => {}
                    2 => writeln!(fmt, "{:<lw$} | {}", lp + 1, self.get_span_prev_line(prev))?,
                    _ => writeln!(fmt, "{:<1$}...", "", lw - 1)?,
                }
            }
            _ => {
                writeln!(fmt, "{:<lw$} |", ' ')?;
            }
        }

        writeln!(fmt, "{:<lw$} | {}", ln, self.get_err_line())?;
        write!(fmt, "{:<lw$} | {:col$}{:^<width$}", ' ', "", '^')?;

        match self.err.kind {
            ErrorKind::Redefined(_, prev) => {
                write!(fmt, " `{}` redefined here", *prev)?;
            }
            ErrorKind::Undefined(_) => {
                write!(fmt, " not found")?;
            }
            ErrorKind::FieldLenZero => {
                write!(fmt, " must not be zero")?;
            }
            ErrorKind::FieldPos(pos, bits) => {
                write!(
                    fmt,
                    " offset is {pos} but the instruction is {bits} bits wide"
                )?;
            }
            _ => {}
        }

        writeln!(fmt)
    }
}

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
        e.or_kind(Expected::Char(c))
    })
}

fn tag<'a>(s: &'static str) -> impl FnMut(Span<'a>) -> IResult<'a> {
    map_err(nom::bytes::complete::tag(s), move |e| {
        e.or_kind(Expected::Tag(s))
    })
}

fn line_break(s: Span) -> IResult<()> {
    value((), delimited(space0, pair(char('\\'), line_ending), space0))(s)
}

fn sp0(s: Span) -> IResult<()> {
    map_err(
        value((), many0_count(alt((line_break, value((), space1))))),
        |e| e.or_kind(Expected::Space),
    )(s)
}

fn sp1(s: Span) -> IResult<()> {
    map_err(
        value((), many1_count(alt((line_break, value((), space1))))),
        |e| e.or_kind(Expected::Space),
    )(s)
}

fn line_comment(s: Span) -> IResult<()> {
    value((), preceded(char('#'), not_line_ending))(s)
}

fn whitespace0(s: Span) -> IResult<()> {
    value(
        (),
        terminated(
            many0_count(preceded(multispace0, line_comment)),
            multispace0,
        ),
    )(s)
}

fn eol(s: Span) -> IResult<()> {
    map_err(
        value(
            (),
            preceded(pair(sp0, opt(line_comment)), alt((line_ending, eof))),
        ),
        |_| Error::new(s.take(0), ErrorKind::Unexpected),
    )(s)
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
struct Number<'a, T> {
    value: T,
    span: Span<'a>,
}

impl<'a, T> Number<'a, T> {
    #[cfg(test)]
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
        |_| Error::new(s.take(0), Expected::Number),
    )(s)
}

fn identifier(s: Span) -> IResult {
    let first = map_err(satisfy(|c: char| c.is_alphabetic() || c == '_'), |e| {
        e.or_kind(Expected::Identifier)
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

fn args_ref(s: Span) -> IResult {
    args_name(s)
}

fn format_name(s: Span) -> IResult {
    preceded_name('@')(s)
}

fn format_ref(s: Span) -> IResult {
    format_name(s)
}

fn s_len<T: FromStrRadix>(s: Span) -> IResult<(bool, Number<T>)> {
    let signed = alt((value(true, char('s')), success(false)));
    map_fail(pair(signed, cut(number)), |e| {
        e.or_kind(Expected::NumberOrS)
    })(s)
}

fn parse_field<'a, T, F>(f: F) -> impl FnMut(Span<'a>) -> IResult<'a, (T, bool, Number<'a, u32>)>
where
    F: FnMut(Span<'a>) -> IResult<'a, T>,
{
    map(separated_pair(f, char(':'), s_len), |(p, (s, l))| (p, s, l))
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct UnnamedField<'a> {
    pos: Number<'a, u32>,
    len: Number<'a, u32>,
    sxt: bool,
}

impl<'a> UnnamedField<'a> {
    fn new(pos: Number<'a, u32>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self { pos, len, sxt }
    }

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
    name: Span<'a>,
    len: Number<'a, u32>,
    sxt: bool,
}

impl<'a> NamedField<'a> {
    fn new(name: Span<'a>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self { name, len, sxt }
    }

    pub fn name(&self) -> &'a str {
        &self.name
    }

    pub fn len(&self) -> u32 {
        self.len.value
    }

    pub fn sign_extend(&self) -> bool {
        self.sxt
    }
}

fn unnamed_field(s: Span) -> IResult<UnnamedField> {
    map(parse_field(number), |(pos, sxt, len)| {
        UnnamedField::new(pos, len, sxt)
    })(s)
}

fn named_field(s: Span) -> IResult<NamedField> {
    map(parse_field(identifier), |(name, sxt, len)| {
        NamedField::new(name, len, sxt)
    })(s)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Field<'a> {
    Unnamed(UnnamedField<'a>),
    Named(NamedField<'a>),
}

impl<'a> Field<'a> {
    #[cfg(test)]
    fn new_unnamed(pos: Number<'a, u32>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self::Unnamed(UnnamedField { pos, len, sxt })
    }
}

fn field(s: Span) -> IResult<Field> {
    let unnamed = map(unnamed_field, Field::Unnamed);
    let named = map(named_field, Field::Named);
    map_err(alt((unnamed, named)), |e| {
        e.or_kind(Expected::NumberOrIdentifier)
    })(s)
}

fn field_fn(s: Span) -> IResult<&str> {
    let (s, _) = char('!')(s)?;
    let prefix = pair(tag("function"), char('='));
    cut(preceded(prefix, map(identifier, |s| *s)))(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldDef<'a> {
    pub name: Span<'a>,
    pub func: Option<&'a str>,
    pub items: Vec<Field<'a>>,
}

fn field_def(s: Span) -> IResult<FieldDef> {
    let (s, name) = field_name(s)?;
    let (s, items) = many0(preceded(sp1, field))(s)?;
    let (s, func) = opt(preceded(sp1, field_fn))(s)?;
    Ok((s, FieldDef { name, func, items }))
}

fn arg_type(s: Span) -> IResult<&str> {
    map_err(map(identifier, |s| *s), |e| e.or_kind(Expected::ArgType))(s)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Arg<'a> {
    name: Span<'a>,
    ty: Option<&'a str>,
}

impl<'a> Arg<'a> {
    fn new(name: Span<'a>, ty: Option<&'a str>) -> Self {
        Self { name, ty }
    }

    pub fn name(&self) -> &'a str {
        &self.name
    }

    pub fn ty(&self) -> Option<&'a str> {
        self.ty
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FormatItem<'a> {
    FixedBits(Span<'a>),
    Field(NamedField<'a>),
    FieldRef(FieldRef<'a>),
    ArgsRef(Span<'a>),
    Const(Const<'a>),
}

#[cfg(test)]
impl<'a> FormatItem<'a> {
    fn fixed_bits(value: Span<'a>) -> Self {
        Self::FixedBits(value)
    }

    fn args_ref(name: Span<'a>) -> Self {
        Self::ArgsRef(name)
    }

    fn field(name: Span<'a>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self::Field(NamedField { name, len, sxt })
    }

    fn field_ref(name: Span<'a>, field: Span<'a>) -> Self {
        Self::FieldRef(FieldRef { name, field })
    }

    fn const_val(name: Span<'a>, num: Number<'a, i64>) -> Self {
        Self::Const(Const { name, num })
    }
}

fn format_item(s: Span) -> IResult<FormatItem> {
    let p = alt((
        map(fixedbits, FormatItem::FixedBits),
        map(format_field, FormatItem::Field),
        map(field_ref, FormatItem::FieldRef),
        map(args_ref, FormatItem::ArgsRef),
        map(const_value, FormatItem::Const),
    ));
    map_err(p, |e| {
        e.or_kind(Expected::Msg("fixedbit, field, field_ref or args_ref"))
    })(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FormatDef<'a> {
    pub name: Span<'a>,
    pub items: Vec<FormatItem<'a>>,
}

impl<'a> FormatDef<'a> {
    fn new(name: Span<'a>, items: Vec<FormatItem<'a>>) -> Self {
        Self { name, items }
    }
}

fn format_def(s: Span) -> IResult<FormatDef> {
    let (s, name) = format_name(s)?;
    let (s, items) = cut(many1(preceded(sp1, format_item)))(s)?;
    Ok((s, FormatDef::new(name, items)))
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Const<'a> {
    name: Span<'a>,
    num: Number<'a, i64>,
}

impl<'a> Const<'a> {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> i64 {
        self.num.value
    }
}

fn const_value(s: Span) -> IResult<Const> {
    map(
        separated_pair(identifier, char('='), cut(number)),
        |(name, num)| Const { name, num },
    )(s)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternItem<'a> {
    ArgsRef(Span<'a>),
    FormatRef(Span<'a>),
    FixedBits(Span<'a>),
    Field(NamedField<'a>),
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
        Self::ArgsRef(name)
    }

    fn field(name: Span<'a>, len: Number<'a, u32>, sxt: bool) -> Self {
        Self::Field(NamedField { name, len, sxt })
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
        map(args_ref, PatternItem::ArgsRef),
        map(format_ref, PatternItem::FormatRef),
        map(fixedbits, PatternItem::FixedBits),
        map(format_field, PatternItem::Field),
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
    pub name: Span<'a>,
    pub items: Vec<PatternItem<'a>>,
}

impl<'a> PatternDef<'a> {
    fn new(name: Span<'a>, items: Vec<PatternItem<'a>>) -> Self {
        Self { name, items }
    }
}

fn pattern_def(s: Span) -> IResult<PatternDef> {
    let (s, name) = identifier(s)?;
    let (s, items) = cut(many1(preceded(sp1, pattern_item)))(s)?;
    Ok((s, PatternDef::new(name, items)))
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Group<'a> {
    pub overlap: bool,
    pub items: Vec<GroupItem<'a>>,
}

fn group_delimited<'a>(
    open: char,
    close: char,
    overlap: bool,
) -> impl FnMut(Span<'a>) -> IResult<'a, Group<'a>> {
    delimited(
        preceded(whitespace0, char(open)),
        map(cut(many1(group_item)), move |items| Group {
            overlap,
            items,
        }),
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
enum Stmt<'a> {
    FieldDef(FieldDef<'a>),
    ArgsDef(ArgsDef<'a>),
    FormatDef(FormatDef<'a>),
    PatternDef(PatternDef<'a>),
    Group(Group<'a>),
}

fn stmt(s: Span) -> IResult<Stmt> {
    delimited(
        whitespace0,
        alt((
            map(field_def, Stmt::FieldDef),
            map(args_def, Stmt::ArgsDef),
            map(format_def, Stmt::FormatDef),
            map(pattern_def, Stmt::PatternDef),
            map(group, Stmt::Group),
        )),
        eol,
    )(s)
}

#[derive(Clone, Debug, Default)]
pub struct Errors<'a> {
    src: &'a str,
    list: Vec<Error<'a>>,
}

impl<'a> Errors<'a> {
    fn push(&mut self, span: Span<'a>, kind: ErrorKind<'a>) {
        self.list.push(Error::new(span, kind));
    }

    fn redefined(&mut self, token: Token, cur: Span<'a>, prev: Span<'a>) {
        self.push(cur, ErrorKind::Redefined(token, prev));
    }

    fn undefined(&mut self, cur: Span<'a>, token: Token) {
        self.push(cur, ErrorKind::Undefined(token));
    }

    pub fn iter<'b: 'a>(&'b self, file: &'b str) -> impl Iterator<Item = ErrorPrinter<'a>> {
        self.list.iter().map(move |i| i.printer(file, self.src))
    }
}

#[derive(Clone, Debug)]
pub struct Data<'a> {
    pub fields: HashMap<&'a str, FieldDef<'a>>,
    pub args: HashMap<&'a str, ArgsDef<'a>>,
    pub formats: HashMap<&'a str, FormatDef<'a>>,
    pub root: Group<'a>,
}

impl Default for Data<'_> {
    fn default() -> Self {
        Self {
            fields: Default::default(),
            args: Default::default(),
            formats: Default::default(),
            root: Group {
                overlap: false,
                items: Default::default(),
            },
        }
    }
}

impl<'a> Data<'a> {
    pub fn parse(insn_bits: u32, src: &'a str) -> Result<Self, Errors> {
        let mut parser = Parser {
            src,
            insn_bits,
            tree: Default::default(),
            errors: Errors {
                src,
                list: Default::default(),
            },
        };
        parser.parse();

        if parser.errors.list.is_empty() {
            Ok(parser.tree)
        } else {
            Err(parser.errors)
        }
    }
}

pub fn parse(insn_bits: u32, src: &str) -> Result<Data, Errors> {
    Data::parse(insn_bits, src)
}

struct Parser<'a> {
    src: &'a str,
    insn_bits: u32,
    tree: Data<'a>,
    errors: Errors<'a>,
}

impl<'a> Parser<'a> {
    fn check_field(&mut self, span: Span<'a>) {
        if !self.tree.fields.contains_key(*span) {
            self.errors.undefined(span, Token::Field);
        }
    }

    fn check_field_range(&mut self, pos: Number<'a, u32>, len: Number<'a, u32>) {
        if len.value == 0 {
            self.errors.push(len.span, ErrorKind::FieldLenZero);
        }
        if pos.value >= self.insn_bits {
            self.errors
                .push(pos.span, ErrorKind::FieldPos(pos.value, self.insn_bits));
        } else if pos.value + len.value > self.insn_bits {
            self.errors.push(
                len.span,
                ErrorKind::FieldPos(pos.value + len.value, self.insn_bits),
            );
        }
    }

    fn check_args(&mut self, name: &Span<'a>) {
        if !self.tree.args.contains_key(name.fragment()) {
            self.errors.undefined(*name, Token::Args);
        }
    }

    fn check_format(&mut self, name: &Span<'a>) {
        if !self.tree.formats.contains_key(name.fragment()) {
            self.errors.undefined(*name, Token::Format);
        }
    }

    fn add_field_def(&mut self, def: FieldDef<'a>) {
        for i in &def.items {
            match i {
                Field::Unnamed(field) => {
                    self.check_field_range(field.pos, field.len);
                }
                Field::Named(field) => {
                    self.check_field(field.name);
                }
            }
        }

        match self.tree.fields.entry(*def.name) {
            Entry::Vacant(e) => {
                e.insert(def);
            }
            Entry::Occupied(e) => {
                self.errors.redefined(Token::Field, def.name, e.get().name);
            }
        }
    }

    fn add_args_def(&mut self, def: ArgsDef<'a>) {
        match self.tree.args.entry(*def.name) {
            Entry::Vacant(e) => {
                e.insert(def);
            }
            Entry::Occupied(e) => {
                self.errors.redefined(Token::Args, def.name, e.get().name);
            }
        }
    }

    fn add_format_def(&mut self, def: FormatDef<'a>) {
        for i in &def.items {
            match i {
                FormatItem::Field(_) => {
                    // TODO: check collisions
                }
                FormatItem::FieldRef(i) => {
                    self.check_field(i.field);
                }
                FormatItem::ArgsRef(i) => {
                    self.check_args(i);
                }
                FormatItem::FixedBits(_) => {}
                FormatItem::Const(_) => {
                    // TODO: check collisions
                }
            }
        }

        match self.tree.formats.entry(*def.name) {
            Entry::Vacant(e) => {
                e.insert(def);
            }
            Entry::Occupied(e) => {
                self.errors.redefined(Token::Format, def.name, e.get().name);
            }
        }
    }

    fn check_pattern_def(&mut self, def: &PatternDef<'a>) {
        // TODO: check if pattern have the same arguments

        for i in &def.items {
            match i {
                PatternItem::Field(_) => {
                    // TODO: check collisions
                }
                PatternItem::FieldRef(field) => {
                    self.check_field(field.name);
                }
                PatternItem::ArgsRef(args) => {
                    self.check_args(args);
                }
                PatternItem::FormatRef(format) => {
                    self.check_format(format);
                }
                PatternItem::FixedBits(_) => {}
                PatternItem::Const(_) => {
                    // TODO: check collisions
                }
            }
        }
    }

    fn add_pattern_def(&mut self, def: PatternDef<'a>) {
        self.check_pattern_def(&def);
        self.tree.root.items.push(GroupItem::PatternDef(def));
    }

    fn check_group_patterns(&mut self, group: &Group<'a>) {
        for i in &group.items {
            match i {
                GroupItem::PatternDef(def) => self.check_pattern_def(def),
                GroupItem::Group(group) => self.check_group_patterns(group),
            }
        }
    }

    fn add_group(&mut self, group: Group<'a>) {
        self.check_group_patterns(&group);
        self.tree.root.items.push(GroupItem::Group(Box::new(group)));
    }

    fn parse(&mut self) {
        let mut cur = Span::new(self.src);

        while !cur.is_empty() {
            match stmt(cur).finish() {
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
                    self.errors.list.push(err);
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
#[rustfmt::skip]
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
                func: Some("func"),
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
                    Arg::new(span!(s[10; 3]), Some("bar")),
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
                name: span!(s[1; 4]),
                items: vec![
                    FormatItem::fixed_bits(span!(s[6; 4])),
                    FormatItem::field(span!(s[11; 1]), num!(s[14; 1]), true),
                    FormatItem::field_ref(span!(s[17; 1]), span!(s[17; 1])),
                    FormatItem::field_ref(span!(s[19; 1]), span!(s[22; 1])),
                    FormatItem::args_ref(span!(s[25; 4])),
                    FormatItem::const_val(span!(s[30; 1]), num!(s[32; 2])),
                ],
            }
        )), format_def(s.into()));
    }

    #[test]
    fn parse_pattern_def() {
        let s = "name 01.- a:s4 %b c=%d &args @e f=13";
        assert_eq!(Ok((
            span!(s[36; 0]),
            PatternDef {
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
            }
        )), pattern_def(s.into()));
    }
}
