use std::{cmp, fmt};

use nom::{
    error::{ErrorKind as NomErrorKind, FromExternalError, ParseError},
    InputTake,
};

use crate::parser::Span;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Expected {
    Identifier,
    IdentifierOrTilde,
    Hexadecimal,
    Decimal,
    Number,
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
            Self::IdentifierOrTilde => write!(fmt, "`~` or identifier"),
            Self::Hexadecimal => write!(fmt, "hexadecimal number"),
            Self::Decimal => write!(fmt, "decimal number"),
            Self::Number => write!(fmt, "number"),
            Self::NumberOrS => write!(fmt, "`s` or number"),
            Self::Space => write!(fmt, "space"),
            Self::ArgType => write!(fmt, "arg type"),
            Self::Char(c) => write!(fmt, "char `{c}`"),
            Self::Tag(s) => write!(fmt, "`{s}`"),
            Self::Msg(s) => write!(fmt, "{s}"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token {
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
pub enum ErrorKind<'a> {
    Expected(Expected),
    Unexpected,
    FieldLenZero,
    FieldPos(u32, u32),
    Redefined(Token, Span<'a>),
    Undefined(Token),
    UndefinedMember(Span<'a>, Span<'a>),
    Overflow(u32, u32),
    InsnSize(u32, u32),
    Overlap(Span<'a>),
    InvalidOpcode,
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
pub struct Error<'a> {
    pub(crate) input: Span<'a>,
    pub(crate) kind: ErrorKind<'a>,
}

impl<'a> Error<'a> {
    pub fn new<K: Into<ErrorKind<'a>>>(input: Span<'a>, kind: K) -> Self {
        Self {
            input,
            kind: kind.into(),
        }
    }

    pub fn or_kind<K: Into<ErrorKind<'a>>>(self, kind: K) -> Self {
        Self {
            kind: kind.into(),
            ..self
        }
    }

    pub fn or_kind_take<K, F>(self, kind: K, mut f: F) -> Self
    where
        K: Into<ErrorKind<'a>>,
        F: FnMut(char) -> bool,
    {
        let len = self
            .input
            .char_indices()
            .find(|(_, c)| f(*c))
            .map(|(i, _)| i)
            .unwrap_or(0);

        Self {
            input: self.input.take(len),
            kind: kind.into(),
        }
    }

    pub fn printer<'b: 'a>(&'b self, file: &'a str, src: &'a str) -> ErrorPrinter<'b> {
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
            E::UndefinedMember(_, name) => write!(fmt, "undefined member `{name}`"),
            E::Nom(i) => write!(fmt, "{i:?}"),
            E::FieldLenZero => write!(fmt, "field length is zero"),
            E::FieldPos(_, _) => write!(fmt, "field offset is outside the width of instruction"),
            E::Overflow(insn_size, len) => write!(
                fmt,
                "instruction size overflow, expected {insn_size} bits but the size is {len} bits"
            ),
            E::InsnSize(insn_size, len) => write!(
                fmt,
                "instruction size must be {insn_size} bits but got {len} bits"
            ),
            E::Overlap(..) => write!(fmt, "pattern overlap"),
            E::InvalidOpcode => write!(fmt, "opcode differs for shared part in an overlap group"),
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

    fn get_span_next_line(&self, span: Span<'a>) -> &'a str {
        let offset = span.location_offset();
        let start = self.src[offset..]
            .char_indices()
            .find(|(_, c)| *c == '\n')
            .map(|(i, _)| offset + i + 1)
            .unwrap();
        let end = self.src[start..]
            .char_indices()
            .find(|(_, c)| *c == '\n')
            .map(|(i, _)| start + i)
            .unwrap_or(self.src.len());
        &self.src[start..end]
    }

    fn join(&self, fmt: &mut fmt::Formatter, lw: usize, l1: Span<'_>, l2: Span<'_>) -> fmt::Result {
        let ln1 = l1.location_line();
        let ln2 = l2.location_line();

        let (prev, lp, ln) = if ln1 < ln2 {
            (l1, ln1, ln2)
        } else {
            (l2, ln2, ln1)
        };

        match ln - lp {
            0 | 1 => Ok(()),
            2 => writeln!(fmt, "{:<lw$} | {}", lp + 1, self.get_span_next_line(prev)),
            _ => writeln!(fmt, "{:<1$}...", "", lw - 1),
        }
    }

    fn line(
        &self,
        fmt: &mut fmt::Formatter,
        lw: usize,
        line: Span<'a>,
        fill: char,
        msg: &str,
    ) -> fmt::Result {
        let lp = line.location_line();
        let col = line.get_utf8_column() - 1;
        writeln!(fmt, "{:<lw$} | {}", lp, self.get_span_line(line))?;
        write!(fmt, "{:<lw$} | {:col$}", ' ', "")?;
        for _ in 0..cmp::max(1, line.chars().count()) {
            write!(fmt, "{fill}")?;
        }
        writeln!(fmt, " {msg}")?;
        Ok(())
    }
}

impl fmt::Display for ErrorPrinter<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "error: {}", self.err)?;

        let err = self.err.input;
        let err_ln = err.location_line();
        let mut max = err_ln;
        match self.err.kind {
            ErrorKind::Redefined(_, prev) | ErrorKind::Overlap(prev) => {
                max = cmp::max(max, prev.location_line());
            }
            ErrorKind::UndefinedMember(pattern, name) => {
                max = cmp::max(max, name.location_line());
                if max != pattern.location_line() {
                    max = cmp::max(max, pattern.location_line());
                }
            }
            _ => {}
        }

        let lw = self.location_line_width(max);
        let col = err.get_utf8_column() - 1;
        writeln!(fmt, "{:<lw$}--> {}:{err_ln}:{}", ' ', self.file, col + 1)?;

        match self.err.kind {
            ErrorKind::Redefined(token, prev) => {
                let msg = format!("previous definition of the {token} `{}` here", *prev);
                self.line(fmt, lw, prev, '-', &msg)?;
                self.join(fmt, lw, prev, err)?;
            }
            ErrorKind::UndefinedMember(pattern, name) => {
                self.line(fmt, lw, name, '-', "defined here")?;
                let prev = if err_ln > pattern.location_line() {
                    self.join(fmt, lw, name, pattern)?;
                    self.line(fmt, lw, pattern, '-', "required for pattern")?;
                    pattern
                } else {
                    name
                };
                self.join(fmt, lw, prev, err)?;
            }
            ErrorKind::Overlap(prev) => {
                self.line(fmt, lw, prev, '-', "defined here")?;
                self.join(fmt, lw, prev, err)?;
            }
            _ => {
                writeln!(fmt, "{:<lw$} |", ' ')?;
            }
        }

        let msg = match self.err.kind {
            ErrorKind::Redefined(_, prev) => format!("`{}` redefined here", &prev),
            ErrorKind::Undefined(_) => "not found".to_string(),
            ErrorKind::UndefinedMember(..) => "required here".to_string(),
            ErrorKind::FieldLenZero => "must not be zero".to_string(),
            ErrorKind::FieldPos(bits, pos) => {
                format!("offset is {pos} but the instruction is {bits} bits wide")
            }
            ErrorKind::Unexpected | ErrorKind::Expected(..) => "here".to_string(),
            ErrorKind::Overflow(..) | ErrorKind::InvalidOpcode => "defined here".to_string(),
            ErrorKind::Overlap(prev) => format!("overlaps with `{}`", &prev),
            _ => String::new(),
        };
        self.line(fmt, lw, err, '^', &msg)?;

        if let ErrorKind::UndefinedMember(pattern, ..) = self.err.kind {
            if err_ln < pattern.location_line() {
                self.join(fmt, lw, err, pattern)?;
                let msg = format!("required for pattern `{}`", &pattern);
                self.line(fmt, lw, pattern, '-', &msg)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Default)]
pub struct Errors<'a> {
    src: &'a str,
    list: Vec<Error<'a>>,
}

impl<'a> Errors<'a> {
    pub(crate) fn new(src: &'a str) -> Self {
        Self {
            src,
            list: Default::default(),
        }
    }

    pub(crate) fn push_err(&mut self, err: Error<'a>) {
        self.list.push(err);
    }

    pub(crate) fn push(&mut self, span: Span<'a>, kind: ErrorKind<'a>) {
        self.push_err(Error::new(span, kind));
    }

    pub(crate) fn redefined(&mut self, token: Token, cur: Span<'a>, prev: Span<'a>) {
        self.push(cur, ErrorKind::Redefined(token, prev));
    }

    pub(crate) fn undefined(&mut self, cur: Span<'a>, token: Token) {
        self.push(cur, ErrorKind::Undefined(token));
    }

    pub(crate) fn field_pos(&mut self, cur: Span<'a>, insn_size: u32, pos: u32) {
        self.push(cur, ErrorKind::FieldPos(insn_size, pos));
    }

    pub(crate) fn overflow(&mut self, cur: Span<'a>, insn_size: u32, len: u32) {
        self.push(cur, ErrorKind::Overflow(insn_size, len));
    }

    pub(crate) fn insn_size(&mut self, cur: Span<'a>, insn_size: u32, len: u32) {
        self.push(cur, ErrorKind::InsnSize(insn_size, len));
    }

    pub(crate) fn overlap(&mut self, cur: Span<'a>, prev: Span<'a>) {
        self.push(cur, ErrorKind::Overlap(prev));
    }

    pub(crate) fn invalid_opcode(&mut self, cur: Span<'a>) {
        self.push(cur, ErrorKind::InvalidOpcode);
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn iter<'b: 'a>(&'b self, file: &'b str) -> impl Iterator<Item = ErrorPrinter<'a>> {
        self.list.iter().map(move |i| i.printer(file, self.src))
    }
}
