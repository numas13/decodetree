use std::{
    cmp::Ord,
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
    io::{self, Write},
};

use crate::{DecodeTree, Field, FieldItem, Group, Insn, Item, Pattern, ValueKind};

pub struct GeneratorBuilder {
    trait_name: String,
    type_name: Option<String>,
    zextract: String,
    sextract: String,
    stubs: bool,
}

impl Default for GeneratorBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl GeneratorBuilder {
    pub fn new() -> Self {
        Self {
            trait_name: String::from("Decode"),
            type_name: None,
            zextract: String::from("zextract"),
            sextract: String::from("sextract"),
            stubs: false,
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

    pub fn build<T: Insn>(self, tree: &DecodeTree<T>) -> Generator<T> {
        Generator {
            trait_name: self.trait_name,
            type_name: self.type_name.unwrap_or_else(|| format!("u{}", T::width())),
            zextract: self.zextract,
            sextract: self.sextract,
            stubs: self.stubs,
            tree,
        }
    }
}

pub struct Generator<'a, T> {
    trait_name: String,
    type_name: String,
    zextract: String,
    sextract: String,
    stubs: bool,
    tree: &'a DecodeTree<T>,
}

impl<'a, T> Generator<'a, T>
where
    T: fmt::LowerHex + Eq + Ord + Hash + Insn,
{
    pub fn builder() -> GeneratorBuilder {
        GeneratorBuilder::new()
    }

    fn gen_trans_proto_pattern<W: Write>(
        &self,
        out: &mut W,
        pattern: &'a Pattern<T>,
        set: &mut HashSet<&'a str>,
    ) -> io::Result<()> {
        if set.contains(pattern.name.as_str()) {
            return Ok(());
        }

        set.insert(&pattern.name);
        write!(out, "\tfn trans_{}(&mut self", pattern.name)?;
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
        if self.stubs {
            writeln!(out, ") -> bool {{ todo!(\"trans_{}\") }}", pattern.name)
        } else {
            writeln!(out, ") -> bool;")
        }
    }

    fn gen_trans_proto_group<W: Write>(
        &self,
        out: &mut W,
        group: &'a Group<T>,
        set: &mut HashSet<&'a str>,
    ) -> io::Result<()> {
        for i in &group.items {
            self.gen_trans_proto_item(out, i, set)?;
        }
        Ok(())
    }

    fn gen_trans_proto_item<W: Write>(
        &self,
        out: &mut W,
        item: &'a Item<T>,
        set: &mut HashSet<&'a str>,
    ) -> io::Result<()> {
        match item {
            Item::Pattern(ref p) => {
                self.gen_trans_proto_pattern(out, p, set)?;
            }
            Item::Group(ref g) => {
                self.gen_trans_proto_group(out, g, set)?;
            }
        }
        Ok(())
    }

    fn gen_trans_proto<W: Write>(&self, out: &mut W) -> io::Result<()> {
        let mut set = HashSet::new();
        for i in &self.tree.items {
            self.gen_trans_proto_item(out, i, &mut set)?;
        }
        writeln!(out)
    }

    fn gen_args<W: Write>(&self, out: &mut W) -> io::Result<()> {
        for args in self.tree.args.values().filter(|i| !i.is_extern) {
            writeln!(out, "#[allow(non_camel_case_types)]")?;
            if args.items.is_empty() {
                writeln!(out, "pub struct args_{};", args.name)?;
            } else {
                writeln!(out, "pub struct args_{} {{", args.name)?;
                for i in &args.items {
                    let ty = i.ty.as_deref().unwrap_or("isize");
                    writeln!(out, "\tpub {}: {ty},", i.name)?;
                }
                writeln!(out, "}}")?;
            }
            writeln!(out)?;
        }
        Ok(())
    }

    fn gen_user_func_proto<W: Write>(&self, out: &mut W) -> io::Result<()> {
        let mut set = HashSet::new();
        for i in self.tree.fields.values().filter_map(|i| i.func.as_deref()) {
            if !set.contains(i) {
                set.insert(i);
                write!(out, "\tfn {i}(&mut self, value: isize) -> isize")?;
                if self.stubs {
                    writeln!(out, " {{ todo!(\"{i}\") }}")?;
                } else {
                    writeln!(out, ";")?;
                }
            }
        }
        writeln!(out)
    }

    fn gen_extract_filed<W: Write>(&self, out: &mut W, field: &Field, pad: &str) -> io::Result<()> {
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

    fn gen_extract_fields<W: Write>(&self, out: &mut W) -> io::Result<()> {
        for field in self.tree.fields.values() {
            writeln!(
                out,
                "\tfn extract_{}(&mut self, insn: {}) -> isize {{",
                field.name, self.type_name
            )?;
            self.gen_extract_filed(out, field, "\t\t")?;
            writeln!(out, "\t}}",)?;
            writeln!(out)?;
        }
        Ok(())
    }

    fn gen_trans_call<W: Write>(&self, out: &mut W, i: &Pattern<T>, pad: &str) -> io::Result<()> {
        for set in &i.sets {
            writeln!(out, "{pad}let {0} = args_{0} {{", set.name)?;
            for arg in &set.items {
                write!(out, "{pad}\t{}: ", arg.name)?;
                match arg.value.as_ref().unwrap() {
                    ValueKind::Field(f) => {
                        if f.name.is_empty() {
                            writeln!(out, "{{")?;
                            self.gen_extract_filed(out, f, &format!("{pad}\t\t"))?;
                            write!(out, "{pad}\t}}")?;
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
                        self.gen_extract_filed(out, f, &format!("{pad}\t"))?;
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

    fn gen_decode_group<W: Write>(
        &self,
        out: &mut W,
        group: &Group<T>,
        pad: &str,
    ) -> io::Result<()> {
        for i in &group.items {
            match i {
                Item::Pattern(p) => {
                    if p.mask == T::ones() {
                        writeln!(out, "{pad}if insn == {:#x} {{", p.opcode)?;
                    } else {
                        writeln!(out, "{pad}if insn & {:#x} == {:#x} {{", p.mask, p.opcode)?;
                    }
                    self.gen_trans_call(out, p, &format!("{pad}\t"))?;
                    writeln!(out, "{pad}}}")?;
                }
                Item::Group(g) => {
                    self.gen_decode_group(out, g, pad)?;
                }
            }
        }
        Ok(())
    }

    fn gen_decode_patterns<W: Write>(&self, out: &mut W) -> io::Result<()> {
        let mut map = HashMap::<_, Vec<_>>::new();

        for i in &self.tree.items {
            let mask = match i {
                Item::Pattern(p) => &p.mask,
                Item::Group(g) => &g.mask,
            };
            map.entry(mask).or_default().push(i);
        }

        let mut map: Vec<_> = map.into_iter().collect();
        map.sort_by(|a, b| a.0.cmp(b.0).then(a.1.len().cmp(&b.1.len())));

        for (mask, patterns) in map {
            if mask == &T::ones() {
                writeln!(out, "\t\tmatch insn {{")?;
            } else {
                writeln!(out, "\t\tmatch insn & {:#x} {{", mask)?;
            }
            for i in patterns {
                match i {
                    Item::Pattern(p) => {
                        writeln!(out, "\t\t\t{:#x} => {{", p.opcode)?;
                        self.gen_trans_call(out, p, "\t\t\t\t")?;
                        writeln!(out, "\t\t\t}}")?;
                    }
                    Item::Group(g) => {
                        writeln!(out, "\t\t\t{:#x} => {{", g.opcode)?;
                        self.gen_decode_group(out, g, "\t\t\t\t")?;
                        writeln!(out, "\t\t\t}}")?;
                    }
                }
            }
            writeln!(out, "\t\t\t_ => {{}}")?;
            writeln!(out, "\t\t}}")?;
            writeln!(out)?;
        }

        Ok(())
    }

    fn gen_decode<W: Write>(&self, out: &mut W) -> io::Result<()> {
        writeln!(
            out,
            "\tfn decode(&mut self, insn: {}) -> bool {{",
            self.type_name
        )?;
        self.gen_decode_patterns(out)?;
        writeln!(out, "\t\tfalse")?;
        writeln!(out, "\t}}")
    }

    pub fn gen<W: Write>(&self, mut out: W) -> io::Result<()> {
        self.gen_args(&mut out)?;
        writeln!(out, "#[allow(clippy::collapsible_if)]")?;
        writeln!(out, "#[allow(clippy::single_match)]")?;
        if self.stubs {
            writeln!(out, "#[allow(unused_variables)]")?;
        }
        writeln!(out, "pub trait {} {{", self.trait_name)?;
        self.gen_user_func_proto(&mut out)?;
        self.gen_extract_fields(&mut out)?;
        self.gen_trans_proto(&mut out)?;
        self.gen_decode(&mut out)?;
        writeln!(out, "}}")?;
        Ok(())
    }
}
