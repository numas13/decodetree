use std::{
    cmp::Ord,
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
    io::{self, Cursor, Write},
};

use crate::{DecodeTree, Field, FieldItem, Group, Insn, Item, Pattern, ValueKind};

#[derive(Copy, Clone)]
pub struct Pad(usize);

impl Pad {
    pub fn shift(self) -> Self {
        Self(self.0 + 4)
    }

    pub fn shift2(self) -> Self {
        Self(self.0 + 8)
    }
}

impl fmt::Display for Pad {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.0 != 0 {
            write!(fmt, "{1:0$}", self.0, ' ')
        } else {
            Ok(())
        }
    }
}

pub trait Visitor<T> {
    #[allow(unused_variables)]
    #[inline]
    fn visit_trans_proto_pattern<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T>,
    ) -> io::Result<bool> {
        Ok(false)
    }

    #[allow(unused_variables)]
    #[inline]
    fn visit_trait_body<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        Ok(())
    }

    #[allow(unused_variables)]
    #[inline]
    fn visit_end<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        Ok(())
    }
}

impl<T> Visitor<T> for () {}

pub struct GeneratorBuilder<V = ()> {
    trait_name: String,
    type_name: Option<String>,
    zextract: String,
    sextract: String,
    stubs: bool,
    opcodes: bool,
    visitor: Option<V>,
}

impl<V> Default for GeneratorBuilder<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> GeneratorBuilder<V> {
    pub fn new() -> Self {
        Self {
            trait_name: String::from("Decode"),
            type_name: None,
            zextract: String::from("zextract"),
            sextract: String::from("sextract"),
            stubs: false,
            opcodes: false,
            visitor: None,
        }
    }

    pub fn trait_name(mut self, s: &str) -> Self {
        self.trait_name = s.to_owned();
        self
    }

    pub fn type_name(mut self, s: &str) -> Self {
        self.type_name = Some(s.to_owned());
        self
    }

    pub fn zextract(mut self, func: &str) -> Self {
        self.zextract = func.to_owned();
        self
    }

    pub fn sextract(mut self, func: &str) -> Self {
        self.sextract = func.to_owned();
        self
    }

    pub fn stubs(mut self, stubs: bool) -> Self {
        self.stubs = stubs;
        self
    }

    pub fn opcodes(mut self, opcodes: bool) -> Self {
        self.opcodes = opcodes;
        self
    }

    pub fn visitor(mut self, visitor: V) -> Self {
        self.visitor = Some(visitor);
        self
    }

    pub fn build<T: Insn>(self, tree: &DecodeTree<T>) -> Generator<T, V> {
        Generator {
            trait_name: self.trait_name,
            type_name: self.type_name.unwrap_or_else(|| format!("u{}", T::width())),
            zextract: self.zextract,
            sextract: self.sextract,
            stubs: self.stubs,
            opcodes: self.opcodes,
            visitor: self.visitor,
            tree,
        }
    }
}

pub struct Generator<'a, T, V = ()> {
    trait_name: String,
    type_name: String,
    zextract: String,
    sextract: String,
    stubs: bool,
    opcodes: bool,
    visitor: Option<V>,
    tree: &'a DecodeTree<T>,
}

impl<'a, T, V> Generator<'a, T, V>
where
    T: fmt::LowerHex + Eq + Ord + Hash + Insn,
    V: Visitor<T>,
{
    pub fn builder() -> GeneratorBuilder<V> {
        GeneratorBuilder::new()
    }

    fn gen_trans_proto_pattern<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &'a Pattern<T>,
        opcodes: &mut HashSet<&'a str>,
    ) -> io::Result<()> {
        if opcodes.contains(pattern.name.as_str()) {
            return Ok(());
        }

        opcodes.insert(&pattern.name);
        write!(out, "{pad}fn trans_{}(&mut self", pattern.name)?;
        for i in &pattern.sets {
            write!(out, ", {0}: &args_{0}", i.name)?;
        }
        for i in &pattern.args {
            write!(out, ", {0}: ", i.name)?;
            match i.kind {
                ValueKind::Field(_) => write!(out, "isize")?,
                ValueKind::Const(_) => write!(out, "i64")?,
            }
        }
        write!(out, ") -> bool")?;

        if let Some(v) = self.visitor.as_mut() {
            let mut buf = Vec::new();
            if v.visit_trans_proto_pattern(&mut Cursor::new(&mut buf), pad.shift(), pattern)? {
                writeln!(out, " {{")?;
                write!(out, "{}", String::from_utf8(buf).unwrap())?;
                writeln!(out, "{pad}}}")?;
                writeln!(out)?;
                return Ok(());
            }
        }

        if self.stubs {
            writeln!(out, "{{ todo!(\"trans_{}\") }}", pattern.name)?;
        } else {
            writeln!(out, ";")?;
        }

        Ok(())
    }

    fn gen_trans_proto_group<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        group: &'a Group<T>,
        opcodes: &mut HashSet<&'a str>,
    ) -> io::Result<()> {
        for i in &group.items {
            self.gen_trans_proto_item(out, pad, i, opcodes)?;
        }
        Ok(())
    }

    fn gen_trans_proto_item<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        item: &'a Item<T>,
        opcodes: &mut HashSet<&'a str>,
    ) -> io::Result<()> {
        match &item {
            Item::Pattern(p) => {
                self.gen_trans_proto_pattern(out, pad, p, opcodes)?;
            }
            Item::Group(g) => {
                self.gen_trans_proto_group(out, pad, g, opcodes)?;
            }
        }
        Ok(())
    }

    fn gen_args<W: Write>(&self, out: &mut W, pad: Pad) -> io::Result<()> {
        for args in self.tree.args.values().filter(|i| !i.is_extern) {
            writeln!(out, "{pad}#[allow(non_camel_case_types)]")?;
            if args.items.is_empty() {
                writeln!(out, "{pad}pub struct args_{};", args.name)?;
            } else {
                writeln!(out, "{pad}pub struct args_{} {{", args.name)?;
                for i in &args.items {
                    let ty = i.ty.as_deref().unwrap_or("isize");
                    writeln!(out, "{}pub {}: {ty},", pad.shift(), i.name)?;
                }
                writeln!(out, "{pad}}}")?;
            }
            writeln!(out)?;
        }
        Ok(())
    }

    fn gen_user_func_proto<W: Write>(&self, out: &mut W, pad: Pad) -> io::Result<()> {
        let mut set = HashSet::new();
        for i in self.tree.fields.values().filter_map(|i| i.func.as_deref()) {
            if !set.contains(i) {
                set.insert(i);
                write!(out, "{pad}fn {i}(&mut self, value: isize) -> isize")?;
                if self.stubs {
                    writeln!(out, " {{ todo!(\"{i}\") }}")?;
                } else {
                    writeln!(out, ";")?;
                }
                writeln!(out)?;
            }
        }
        Ok(())
    }

    fn gen_extract_filed<W: Write>(&self, out: &mut W, field: &Field, pad: Pad) -> io::Result<()> {
        if field.items.len() == 1 {
            let f = &field.items[0];
            write!(out, "{pad}")?;
            if let Some(func) = field.func.as_deref() {
                write!(out, "self.{func}(")?;
            }
            match f {
                FieldItem::Field { pos, len, sxt } => {
                    let func = if *sxt { &self.sextract } else { &self.zextract };
                    write!(out, "{func}(insn, {pos}, {len}) as isize")?;
                }
                FieldItem::FieldRef { field, len, sxt } => {
                    let func = if *sxt { &self.sextract } else { &self.zextract };
                    write!(
                        out,
                        "{func}(Self::extract_{}(insn), 0, {len}) as isize",
                        field.name
                    )?;
                }
            }
            if field.func.is_some() {
                write!(out, ")")?;
            }
            writeln!(out)?;
        } else {
            writeln!(out, "{pad}let mut out = 0;")?;
            for i in &field.items {
                match i {
                    FieldItem::Field { pos, len, sxt } => {
                        writeln!(out, "{pad}out <<= {};", len)?;
                        let func = if *sxt { &self.sextract } else { &self.zextract };
                        writeln!(out, "{pad}out |= {func}(insn, {pos}, {len}) as isize;")?;
                    }
                    FieldItem::FieldRef { field, len, sxt } => {
                        writeln!(out, "{pad}out <<= {};", len)?;
                        let s = if *sxt { "s" } else { "" };
                        writeln!(
                            out,
                            "{pad}out |= {s}extract(Self::extract_{}(insn), 0, {len}) as isize;",
                            field.name
                        )?;
                    }
                }
            }
            if let Some(func) = &field.func {
                writeln!(out, "{pad}self.{func}(out)")?;
            } else {
                writeln!(out, "{pad}out",)?;
            }
        }
        Ok(())
    }

    fn gen_extract_fields<W: Write>(&self, out: &mut W, pad: Pad) -> io::Result<()> {
        for field in self.tree.fields.values() {
            writeln!(
                out,
                "{pad}fn extract_{}(&mut self, insn: {}) -> isize {{",
                field.name, self.type_name
            )?;
            self.gen_extract_filed(out, field, pad.shift())?;
            writeln!(out, "{pad}}}",)?;
            writeln!(out)?;
        }
        Ok(())
    }

    fn gen_call_trans_func<W: Write>(
        &self,
        out: &mut W,
        i: &Pattern<T>,
        pad: Pad,
    ) -> io::Result<()> {
        for set in &i.sets {
            writeln!(out, "{pad}let {0} = args_{0} {{", set.name)?;
            for arg in &set.items {
                write!(out, "{}{}: ", pad.shift(), arg.name)?;
                match arg.value.as_ref().unwrap() {
                    ValueKind::Field(f) => {
                        if f.name.is_empty() {
                            writeln!(out, "{{")?;
                            self.gen_extract_filed(out, f, pad.shift2())?;
                            write!(out, "{}}}", pad.shift())?;
                        } else {
                            write!(out, "self.extract_{}(insn)", f.name)?;
                        }
                    }
                    ValueKind::Const(v) => {
                        write!(out, "{v}")?;
                    }
                }
                if let Some(ref ty) = arg.ty {
                    write!(out, " as {ty}")?;
                }
                writeln!(out, ",")?;
            }
            writeln!(out, "{pad}}};")?;
        }
        for arg in &i.args {
            write!(out, "{pad}let {} = ", arg.name)?;
            match arg.kind {
                ValueKind::Field(ref f) => {
                    if f.name.is_empty() {
                        writeln!(out, "{{")?;
                        self.gen_extract_filed(out, f, pad.shift())?;
                        write!(out, "{pad}}}")?;
                    } else {
                        write!(out, "self.extract_{}(insn)", f.name)?;
                    }
                }
                ValueKind::Const(v) => {
                    write!(out, "{v}")?;
                }
            }
            writeln!(out, ";")?;
        }
        write!(out, "{pad}if Self::trans_{}(self", i.name)?;
        for set in &i.sets {
            write!(out, ", &{}", set.name)?;
        }
        for arg in &i.args {
            write!(out, ", {}", arg.name)?;
        }
        writeln!(out, ") {{ return true }}")?;
        Ok(())
    }

    fn gen_decode_group_items<W: Write>(
        &self,
        out: &mut W,
        items: Vec<&Item<T>>,
        pad: Pad,
        prev: T,
    ) -> io::Result<()> {
        let root_mask = items
            .iter()
            .fold(T::ones(), |mask, i| mask.bit_and(&i.mask()))
            .bit_andn(&prev);
        let next_mask = prev.bit_or(&root_mask);

        let map = {
            let mut map = HashMap::<_, Vec<_>>::new();
            for i in items {
                map.entry(i.opcode().bit_and(&root_mask))
                    .or_default()
                    .push(i);
            }
            map
        };

        writeln!(
            out,
            "{pad}match insn & {root_mask:#x} {{ // non-overlap group"
        )?;

        let mut map: Vec<_> = map.into_iter().collect();
        map.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.len().cmp(&b.1.len())));

        for (opcode, items) in map {
            let pad = pad.shift();

            let mask = items[0].mask();
            if !items.iter().all(|i| mask == i.mask()) {
                writeln!(out, "{pad}{:#x} => {{", opcode)?;
                self.gen_decode_group_items(out, items, pad.shift(), next_mask)?;
            } else {
                let mut pad = pad;
                write!(out, "{pad}{:#x} ", opcode)?;

                let m = mask.bit_andn(&next_mask);
                if items.len() > 1 {
                    writeln!(out, "=> {{")?;
                    pad = pad.shift();
                    writeln!(out, "{pad}match insn & {m:#x} {{")?;
                } else {
                    if m != T::zero() {
                        let o = items[0].opcode().bit_andn(&next_mask);
                        write!(out, "if insn & {m:#x} == {o:#x} ")?;
                    }
                    writeln!(out, "=> {{")?;
                }

                for i in &items {
                    let pad = if items.len() > 1 { pad.shift() } else { pad };
                    if let Item::Pattern(i) = i {
                        let pad = if items.len() > 1 { pad } else { pad.shift() };
                        writeln!(out, "{pad}// {:#x}:{:#x}", i.mask, i.opcode)?;
                    }
                    if items.len() > 1 {
                        writeln!(out, "{pad}{:#x} => {{", i.opcode().bit_and(&m))?;
                    }
                    match i {
                        Item::Pattern(i) => {
                            self.gen_call_trans_func(out, i, pad.shift())?;
                        }
                        Item::Group(i) => {
                            self.gen_decode_group(out, i, pad.shift(), m.bit_or(&next_mask))?;
                        }
                    }

                    if items.len() > 1 {
                        writeln!(out, "{pad}}}")?;
                    }
                }

                if items.len() > 1 {
                    writeln!(out, "{}_ => {{}}", pad.shift())?;
                    writeln!(out, "{pad}}}")?;
                }
            }
            writeln!(out, "{pad}}}")?;
        }

        writeln!(out, "{}_ => {{}}", pad.shift())?;
        writeln!(out, "{pad}}} // match insn & {root_mask:#x}")?;

        Ok(())
    }

    fn gen_decode_group<W: Write>(
        &self,
        out: &mut W,
        group: &Group<T>,
        pad: Pad,
        prev: T,
    ) -> io::Result<()> {
        if group.overlap {
            writeln!(out, "{pad}// overlap group",)?;
            for i in &group.items {
                match i {
                    Item::Pattern(i) => {
                        writeln!(out, "{pad}// {:#x}:{:#x}", i.mask, i.opcode)?;
                        let m = i.mask.bit_andn(&prev);
                        let p = if m != T::zero() {
                            let o = i.opcode.bit_andn(&prev);
                            writeln!(out, "{pad}if insn & {m:#x} == {o:#x} {{ ")?;
                            pad.shift()
                        } else {
                            pad
                        };
                        self.gen_call_trans_func(out, i, p)?;
                        if m != T::zero() {
                            writeln!(out, "{pad}}}")?;
                        }
                    }
                    Item::Group(i) => {
                        self.gen_decode_group(out, i, pad, prev)?;
                    }
                }
            }
            return Ok(());
        }

        let items = group.items.iter().collect();
        self.gen_decode_group_items(out, items, pad, prev)
    }

    fn gen_decode<W: Write>(&self, out: &mut W, pad: Pad) -> io::Result<()> {
        writeln!(out, "{pad}#[inline(never)]")?;
        writeln!(
            out,
            "{pad}fn decode(&mut self, insn: {}) -> bool {{",
            self.type_name
        )?;
        self.gen_decode_group(out, &self.tree.root, pad.shift(), T::zero())?;
        writeln!(out)?;
        writeln!(out, "{}false", pad.shift())?;
        writeln!(out, "{pad}}}")
    }

    fn gen_trait<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        opcodes: &mut HashSet<&'a str>,
    ) -> io::Result<()> {
        writeln!(out, "#[allow(clippy::collapsible_if)]")?;
        writeln!(out, "#[allow(clippy::single_match)]")?;
        if self.stubs {
            writeln!(out, "#[allow(unused_variables)]")?;
        }
        writeln!(out, "{pad}pub trait {}: Sized {{", self.trait_name)?;
        self.gen_user_func_proto(out, pad.shift())?;
        self.gen_extract_fields(out, pad.shift())?;
        self.gen_trans_proto_group(out, pad.shift(), &self.tree.root, opcodes)?;
        self.gen_decode(out, pad.shift())?;
        if let Some(visitor) = self.visitor.as_mut() {
            visitor.visit_trait_body(out, pad.shift())?;
        }
        writeln!(out, "{pad}}}")?;
        Ok(())
    }

    fn gen_opcodes<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        opcodes: &HashSet<&'a str>,
    ) -> io::Result<()> {
        if self.opcodes {
            writeln!(out)?;
            writeln!(out, "{pad}#[derive(Copy, Clone, Debug, PartialEq, Eq)]")?;
            writeln!(out, "{pad}pub enum Opcode {{")?;
            let mut opcodes: Vec<_> = opcodes.iter().collect();
            opcodes.sort();
            for i in opcodes {
                writeln!(out, "{}{},", pad.shift(), i.to_uppercase())?;
            }
            writeln!(out, "{pad}}}")?;
        }
        Ok(())
    }

    pub fn gen<W: Write>(&mut self, mut out: W) -> io::Result<()> {
        let pad = Pad(0);
        let out = &mut out;

        self.gen_args(out, pad)?;

        let mut opcodes = HashSet::new();
        self.gen_trait(out, pad, &mut opcodes)?;
        self.gen_opcodes(out, pad, &opcodes)?;

        if let Some(visitor) = self.visitor.as_mut() {
            writeln!(out)?;
            visitor.visit_end(out, pad)?;
        }

        Ok(())
    }
}
