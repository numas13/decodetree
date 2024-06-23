use std::{
    cmp::Ord,
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
    io::{self, Write},
    ops::Deref,
};

use crate::{
    ArgsValue, ArgsValueKind, DecodeTree, Field, FieldDef, FieldItem, Group, Insn, Item, Overlap,
    OverlapItem, Pattern, ValueKind,
};

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

#[allow(unused_variables)]
pub trait Gen<T, S = String> {
    fn pass_arg(&self, name: &str) -> bool {
        true
    }

    fn additional_args(&self) -> &[(&str, &str)] {
        &[]
    }

    fn gen_trans_body<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T, S>,
    ) -> io::Result<bool> {
        Ok(false)
    }

    fn gen_on_success<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T, S>,
    ) -> io::Result<()> {
        Ok(())
    }

    fn gen_trait_body<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        Ok(())
    }

    fn gen_opcodes<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        opcodes: &HashSet<&str>,
    ) -> io::Result<()> {
        Ok(())
    }

    fn gen_end<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        Ok(())
    }
}

impl<T> Gen<T> for () {}

pub struct GeneratorBuilder {
    trait_name: String,
    type_name: Option<String>,
    zextract: String,
    sextract: String,
    stubs: bool,
}

impl Default for GeneratorBuilder {
    fn default() -> Self {
        Self {
            trait_name: String::from("Decode"),
            type_name: None,
            zextract: String::from("zextract"),
            sextract: String::from("sextract"),
            stubs: false,
        }
    }
}

impl GeneratorBuilder {
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

    pub fn build<T, S, G>(self, tree: &DecodeTree<T, S>, gen: G) -> Generator<T, S, G>
    where
        T: Insn,
        G: Gen<T, S>,
    {
        Generator {
            trait_name: self.trait_name,
            type_name: self.type_name.unwrap_or_else(|| format!("u{}", T::width())),
            zextract: self.zextract,
            sextract: self.sextract,
            stubs: self.stubs,
            gen,
            tree,
            opcodes: Default::default(),
            conditions: Default::default(),
        }
    }
}

pub struct Generator<'a, T, S = String, G = ()> {
    trait_name: String,
    type_name: String,
    zextract: String,
    sextract: String,
    stubs: bool,
    gen: G,
    tree: &'a DecodeTree<T, S>,
    opcodes: HashSet<&'a str>,
    conditions: HashSet<&'a str>,
}

impl<'a, T, S: 'a, G> Generator<'a, T, S, G>
where
    T: Insn,
    S: Eq + Hash + fmt::Display + Deref<Target = str>,
    G: Gen<T, S>,
{
    pub fn builder() -> GeneratorBuilder {
        GeneratorBuilder::default()
    }

    fn gen_comment<W: Write>(&self, out: &mut W, pad: Pad, msg: &str) -> io::Result<()> {
        let width = 60;
        writeln!(out, "{pad}///{:/<width$}///", "")?;
        writeln!(out, "{pad}// {msg:width$} //")?;
        writeln!(out, "{pad}///{:/<width$}///", "")?;
        writeln!(out)
    }

    fn gen_trans_proto_pattern<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &'a Pattern<T, S>,
    ) -> io::Result<()> {
        if self.opcodes.contains(&*pattern.name) {
            return Ok(());
        }

        self.opcodes.insert(&pattern.name);

        for i in &pattern.cond {
            self.conditions.insert(&i.name);
        }

        write!(out, "{pad}fn trans_{}(&mut self", pattern.name)?;
        for (name, ty) in self.gen.additional_args() {
            write!(out, ", {name}: {ty}")?;
        }
        for value in pattern
            .args
            .iter()
            .filter(|i| self.gen.pass_arg(i.name().as_ref()))
        {
            let name = value.name();
            write!(out, ", {name}: ")?;
            match value.kind() {
                ValueKind::Args(..) => write!(out, "&args_{name}")?,
                ValueKind::Const(..) => write!(out, "i64")?,
                _ => write!(out, "isize")?,
            }
        }
        write!(out, ") -> bool")?;

        if self.gen.gen_trans_body(out, pad, pattern)? {
            writeln!(out)?;
            return Ok(());
        }

        if self.stubs {
            writeln!(out, "{{ todo!(\"trans_{}\") }}", pattern.name)?;
        } else {
            writeln!(out, ";")?;
        }

        Ok(())
    }

    fn gen_trans_proto_overlap_item<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        item: &'a OverlapItem<T, S>,
    ) -> io::Result<()> {
        match &item {
            OverlapItem::Pattern(p) => {
                self.gen_trans_proto_pattern(out, pad, p)?;
            }
            OverlapItem::Group(g) => {
                self.gen_trans_proto_group(out, pad, g)?;
            }
        }
        Ok(())
    }

    fn gen_trans_proto_overlap<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        group: &'a Overlap<T, S>,
    ) -> io::Result<()> {
        for i in &group.items {
            self.gen_trans_proto_overlap_item(out, pad, i)?;
        }
        Ok(())
    }

    fn gen_trans_proto_group_item<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        item: &'a Item<T, S>,
    ) -> io::Result<()> {
        match &item {
            Item::Pattern(p) => {
                self.gen_trans_proto_pattern(out, pad, p)?;
            }
            Item::Overlap(g) => {
                self.gen_trans_proto_overlap(out, pad, g)?;
            }
        }
        Ok(())
    }

    fn gen_trans_proto_group<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        group: &'a Group<T, S>,
    ) -> io::Result<()> {
        for i in &group.items {
            self.gen_trans_proto_group_item(out, pad, i)?;
        }
        Ok(())
    }

    fn gen_cond_proto<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        if self.conditions.is_empty() {
            return Ok(());
        }

        let mut list: Vec<_> = self.conditions.iter().collect();
        list.sort();

        self.gen_comment(out, pad, "Conditions")?;
        for i in &list {
            writeln!(out, "{pad}fn cond_{i}(&self) -> bool;")?;
        }
        writeln!(out)
    }

    fn gen_args<W: Write>(&self, out: &mut W, pad: Pad) -> io::Result<()> {
        self.gen_comment(out, pad, "Argument sets")?;
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

    fn gen_extern_func_proto<W: Write>(&self, out: &mut W, pad: Pad) -> io::Result<()> {
        self.gen_comment(out, pad, "Extern functions")?;
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
            }
        }
        writeln!(out)
    }

    fn gen_extract_field_body<W: Write>(
        &self,
        out: &mut W,
        field: &FieldDef<S>,
        pad: Pad,
    ) -> io::Result<()> {
        if field.items.len() == 1 {
            let f = &field.items[0];
            write!(out, "{pad}")?;
            if let Some(func) = field.func.as_deref() {
                write!(out, "self.{func}(")?;
            }
            match f {
                FieldItem::Field(f) => {
                    let (pos, len) = (f.pos(), f.len());
                    let func = if f.sxt() {
                        &self.sextract
                    } else {
                        &self.zextract
                    };
                    write!(out, "{func}(insn, {pos}, {len}) as isize")?;
                }
                FieldItem::FieldRef(f) => {
                    let (field, len) = (f.field(), f.len());
                    let func = if f.sxt() {
                        &self.sextract
                    } else {
                        &self.zextract
                    };
                    let name = field.name();
                    write!(out, "{func}(Self::extract_{name}(insn), 0, {len}) as isize")?;
                }
            }
            if field.func.is_some() {
                write!(out, ")")?;
            }
            writeln!(out)?;
        } else {
            writeln!(out, "{pad}let mut out = 0;")?;
            for i in field.items() {
                match i {
                    FieldItem::Field(f) => {
                        let (pos, len) = (f.pos(), f.len());
                        writeln!(out, "{pad}out <<= {};", len)?;
                        let func = if f.sxt() {
                            &self.sextract
                        } else {
                            &self.zextract
                        };
                        writeln!(out, "{pad}out |= {func}(insn, {pos}, {len}) as isize;")?;
                    }
                    FieldItem::FieldRef(f) => {
                        let (field, len) = (f.field(), f.len());
                        writeln!(out, "{pad}out <<= {};", len)?;
                        let s = ["", "s"][f.sxt() as usize];
                        let name = field.name();
                        writeln!(out, "{pad}let tmp0 = Self::extract_{name}(insn);")?;
                        writeln!(out, "{pad}out |= {s}extract(tmp0, 0, {len}) as isize;")?;
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
        self.gen_comment(out, pad, "Extract functions")?;
        for field in self.tree.fields.values() {
            let name = field.name();
            let ty = &self.type_name;
            writeln!(
                out,
                "{pad}fn extract_{name}(&mut self, insn: {ty}) -> isize {{",
            )?;
            self.gen_extract_field_body(out, field, pad.shift())?;
            writeln!(out, "{pad}}}",)?;
            writeln!(out)?;
        }
        Ok(())
    }

    fn gen_extract_field<W: Write>(&self, out: &mut W, field: &Field<S>) -> io::Result<()> {
        match field {
            Field::FieldRef(field) => {
                let name = field.name();
                write!(out, "self.extract_{name}(insn)")?;
            }
            Field::Field(field) => {
                let pos = field.pos();
                let len = field.len();
                let func = if field.sxt() {
                    &self.sextract
                } else {
                    &self.zextract
                };
                writeln!(out, "{func}(insn, {pos}, {len}) as isize")?;
            }
        }
        Ok(())
    }

    fn gen_extract_set<W: Write>(
        &self,
        out: &mut W,
        pad: Pad,
        name: &str,
        items: &[ArgsValue<S>],
    ) -> io::Result<()> {
        writeln!(out, "{pad}let {name} = args_{name} {{")?;
        let p = pad.shift();
        for arg in items {
            write!(out, "{p}{}: ", arg.name)?;
            match arg.kind() {
                ArgsValueKind::Field(f) => self.gen_extract_field(out, f)?,
                ArgsValueKind::Const(v) => write!(out, "{v}")?,
            }
            if let Some(ref ty) = arg.ty {
                write!(out, " as {ty}")?;
            }
            writeln!(out, ",")?;
        }
        writeln!(out, "{pad}}};")?;
        Ok(())
    }

    fn gen_extract_args<W: Write>(
        &self,
        out: &mut W,
        i: &Pattern<T, S>,
        pad: Pad,
    ) -> io::Result<()> {
        for arg in i.args.iter().filter(|i| self.gen.pass_arg(i.name())) {
            let name = arg.name();
            match arg.kind() {
                ValueKind::Args(set) => {
                    self.gen_extract_set(out, pad, name, set.as_slice())?;
                }
                ValueKind::Field(f) => {
                    write!(out, "{pad}let {name} = ")?;
                    self.gen_extract_field(out, f)?;
                    writeln!(out, ";")?;
                }
                ValueKind::Const(v) => {
                    writeln!(out, "{pad}let {name} = {v};")?;
                }
            }
        }
        Ok(())
    }

    fn gen_call_trans_func<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T, S>,
    ) -> io::Result<()> {
        self.gen_extract_args(out, pattern, pad)?;
        write!(out, "{pad}if Self::trans_{}(self", pattern.name)?;
        for (name, _) in self.gen.additional_args() {
            write!(out, ", {name}")?;
        }
        for arg in pattern.args.iter().filter(|i| self.gen.pass_arg(i.name())) {
            write!(out, ", {}{}", ["", "&"][arg.is_set() as usize], arg.name())?;
        }
        writeln!(out, ") {{")?;
        self.gen.gen_on_success(out, pad.shift(), pattern)?;
        writeln!(out, "{}return true;", pad.shift())?;
        writeln!(out, "{pad}}}")?;
        Ok(())
    }

    fn gen_decode_group_items<W: Write>(
        &mut self,
        out: &mut W,
        items: Vec<&Item<T, S>>,
        pad: Pad,
        prev: T,
    ) -> io::Result<()> {
        let root_mask = items
            .iter()
            .fold(T::ones(), |mask, i| mask.bit_and(i.mask()))
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
                writeln!(out, "=> {{")?;
                pad = pad.shift();
                writeln!(out, "{pad}match insn & {m:#x} {{")?;

                for i in &items {
                    let pad = pad.shift();
                    match i {
                        Item::Pattern(pat) => {
                            // TODO: comment file:line
                            writeln!(out, "{pad}// {:#x}:{:#x}", pat.mask, pat.opcode)?;
                            write!(out, "{pad}{:#x}", pat.opcode.bit_and(&m))?;
                            for (i, cond) in pat.cond.iter().enumerate() {
                                write!(out, " {} ", if i == 0 { "if" } else { "&&" })?;
                                let inv = if cond.invert { "!" } else { "" };
                                write!(out, "{inv}self.cond_{}()", cond.name)?;
                            }
                            writeln!(out, " => {{")?;
                            self.gen_call_trans_func(out, pad.shift(), pat)?;
                            writeln!(out, "{pad}}}")?;
                        }
                        Item::Overlap(group) => {
                            writeln!(out, "{pad}{:#x} => {{", group.opcode.bit_and(&m))?;
                            self.gen_decode_overlap(out, pad.shift(), group, m.bit_or(&next_mask))?;
                            writeln!(out, "{pad}}}")?;
                        }
                    }
                }

                writeln!(out, "{}_ => {{}}", pad.shift())?;
                writeln!(out, "{pad}}}")?;
            }
            writeln!(out, "{pad}}}")?;
        }

        writeln!(out, "{}_ => {{}}", pad.shift())?;
        writeln!(out, "{pad}}} // match insn & {root_mask:#x}")?;

        Ok(())
    }

    fn gen_decode_overlap<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        group: &Overlap<T, S>,
        prev: T,
    ) -> io::Result<()> {
        writeln!(out, "{pad}// overlap group",)?;
        for i in &group.items {
            match i {
                OverlapItem::Pattern(pat) => {
                    writeln!(out, "{pad}// {:#x}:{:#x}", pat.mask, pat.opcode)?;
                    let m = pat.mask.bit_andn(&prev);
                    let o = pat.opcode.bit_andn(&prev);
                    write!(out, "{pad}if ")?;
                    for cond in &pat.cond {
                        let inv = if cond.invert { "!" } else { "" };
                        write!(out, "{inv}self.cond_{}() && ", cond.name)?;
                    }
                    writeln!(out, "insn & {m:#x} == {o:#x} {{ ")?;
                    self.gen_call_trans_func(out, pad.shift(), pat)?;
                    writeln!(out, "{pad}}}")?;
                }
                OverlapItem::Group(group) => {
                    self.gen_decode_group(out, pad, group, prev)?;
                }
            }
        }
        Ok(())
    }

    fn gen_decode_group<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        group: &Group<T, S>,
        prev: T,
    ) -> io::Result<()> {
        let items = group.items.iter().collect();
        self.gen_decode_group_items(out, items, pad, prev)
    }

    fn gen_decode<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        writeln!(out, "{pad}#[inline(never)]")?;
        write!(out, "{pad}fn decode(&mut self, insn: {}", self.type_name)?;
        for (name, ty) in self.gen.additional_args() {
            write!(out, ", {name}: {ty}")?;
        }
        writeln!(out, ") -> bool {{")?;
        self.gen_decode_group(out, pad.shift(), &self.tree.root, T::zero())?;
        writeln!(out)?;
        writeln!(out, "{}false", pad.shift())?;
        writeln!(out, "{pad}}}")
    }

    fn gen_trait<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        // TODO: fix clippy lints for generated code
        writeln!(out, "#[allow(clippy::collapsible_if)]")?;
        writeln!(out, "#[allow(clippy::single_match)]")?;
        writeln!(out, "#[allow(clippy::erasing_op)]")?;
        writeln!(out, "#[allow(clippy::bad_bit_mask)]")?;
        writeln!(out, "#[allow(clippy::too_many_arguments)]")?;
        writeln!(out, "#[allow(clippy::unnecessary_cast)]")?;
        if self.stubs {
            writeln!(out, "#[allow(unused_variables)]")?;
        }
        writeln!(out, "{pad}pub trait {}: Sized {{", self.trait_name)?;
        let p = pad.shift();
        self.gen_extern_func_proto(out, p)?;
        self.gen_extract_fields(out, p)?;
        self.gen_comment(out, p, "Translations")?;
        self.gen_trans_proto_group(out, p, &self.tree.root)?;
        self.gen_cond_proto(out, p)?;
        self.gen_comment(out, p, "Decode function")?;
        self.gen_decode(out, p)?;
        writeln!(out)?;
        self.gen_comment(out, p, "Extern gen trait body")?;
        self.gen.gen_trait_body(out, p)?;
        writeln!(out, "{pad}}}")
    }

    pub fn generate<W: Write>(&mut self, mut out: W) -> io::Result<()> {
        let pad = Pad(0);
        let out = &mut out;
        self.gen_args(out, pad)?;
        self.gen_trait(out, pad)?;
        writeln!(out)?;
        self.gen_comment(out, pad, "Extern gen opcodes")?;
        self.gen.gen_opcodes(out, pad, &self.opcodes)?;
        self.gen_comment(out, pad, "Extern gen end")?;
        self.gen.gen_end(out, pad)
    }
}
