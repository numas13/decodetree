//! Code generator for decodetree.

use std::{
    collections::HashSet,
    fmt,
    hash::Hash,
    io::{self, Write},
    ops::Deref,
};

use crate::{
    DecodeTree, Field, FieldDef, FieldItem, Group, Insn, Item, Overlap, OverlapItem, Pattern,
    SetValue, SetValueKind, Str, ValueKind,
};

/// Helper to align generated code.
#[derive(Copy, Clone)]
pub struct Pad(usize);

impl Pad {
    /// Shift padding to the right.
    pub fn shift(self) -> Self {
        Self(self.0 + 4)
    }

    /// Shift padding to the right in-place.
    pub fn right(&mut self) -> Self {
        self.0 += 4;
        *self
    }

    /// Shift padding to the left in-place.
    pub fn left(&mut self) -> Self {
        assert!(self.0 >= 4);
        self.0 -= 4;
        *self
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

/// Hooks to generate custom code.
#[allow(unused_variables)]
pub trait Gen<T, S = Str> {
    /// Additional attributes for trait.
    fn trait_attrs(&self) -> &[&str] {
        &[]
    }

    /// Use to generate code inside trait.
    fn trait_body<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        Ok(())
    }

    /// Additional attributes for trans functions.
    fn trans_attrs(&self) -> &[&str] {
        &[]
    }

    /// Filter arguments to trans functions.
    fn trans_check_arg(&self, name: &str) -> bool {
        true
    }

    /// Additional arguments to pass to trans functions.
    fn trans_args(&self) -> &[(&str, &str)] {
        &[]
    }

    /// Use to generate default implementation of trans functions.
    fn trans_body<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T, S>,
    ) -> io::Result<bool> {
        Ok(false)
    }

    /// Use to generate code after successful trans function.
    fn trans_success<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T, S>,
    ) -> io::Result<()> {
        Ok(())
    }

    /// Use to generate code outside of the Decode trait.
    fn end<W: Write>(&mut self, out: &mut W, pad: Pad, opcodes: &HashSet<&str>) -> io::Result<()> {
        Ok(())
    }
}

impl<T> Gen<T> for () {}

/// Generator builder.
pub struct GeneratorBuilder {
    trait_name: Str,
    type_name: Option<Str>,
    zextract: Str,
    sextract: Str,
    stubs: bool,
}

impl Default for GeneratorBuilder {
    fn default() -> Self {
        Self {
            trait_name: Str::from("Decode"),
            type_name: None,
            zextract: Str::from("zextract"),
            sextract: Str::from("sextract"),
            stubs: false,
        }
    }
}

impl GeneratorBuilder {
    /// Override Decode trait name.
    pub fn trait_name(mut self, s: &str) -> Self {
        self.trait_name = s.into();
        self
    }

    /// Override instruction type.
    pub fn insn_type(mut self, s: &str) -> Self {
        self.type_name = Some(s.into());
        self
    }

    /// Override zero-extend extract function.
    pub fn zextract(mut self, name: &str) -> Self {
        self.zextract = name.into();
        self
    }

    /// Override sign-extend extract function.
    pub fn sextract(mut self, name: &str) -> Self {
        self.sextract = name.into();
        self
    }

    /// Generate default functions implementations.
    pub fn stubs(mut self, stubs: bool) -> Self {
        self.stubs = stubs;
        self
    }

    /// Build the `Generator`.
    pub fn build<T, S, G>(self, tree: &DecodeTree<T, S>, gen: G) -> Generator<T, S, G>
    where
        T: Insn,
        G: Gen<T, S>,
    {
        Generator {
            trait_name: self.trait_name,
            type_name: self
                .type_name
                .unwrap_or_else(|| format!("u{}", T::width()).into()),
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

/// Decodetree generator.
pub struct Generator<'a, T = super::DefaultInsn, S = Str, G = ()> {
    trait_name: Str,
    type_name: Str,
    zextract: Str,
    sextract: Str,
    stubs: bool,
    gen: G,
    tree: &'a DecodeTree<T, S>,
    opcodes: HashSet<&'a str>,
    conditions: HashSet<&'a str>,
}

impl Generator<'_> {
    /// Create builder object for `Generator`.
    pub fn builder() -> GeneratorBuilder {
        GeneratorBuilder::default()
    }
}

impl<'a, T, S: 'a, G> Generator<'a, T, S, G>
where
    T: Insn,
    S: Eq + Hash + fmt::Display + Deref<Target = str>,
    G: Gen<T, S>,
{
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
        for (name, ty) in self.gen.trans_args() {
            write!(out, ", {name}: {ty}")?;
        }
        for value in pattern
            .args
            .iter()
            .filter(|i| self.gen.trans_check_arg(i.name().as_ref()))
        {
            let name = value.name();
            write!(out, ", {name}: ")?;
            match value.kind() {
                ValueKind::Set(..) => write!(out, "args_{name}")?,
                ValueKind::Const(..) => write!(out, "i64")?,
                _ => write!(out, "isize")?,
            }
        }
        write!(out, ") -> bool")?;

        if self.gen.trans_body(out, pad, pattern)? {
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
                for i in &g.items {
                    self.gen_trans_proto_overlap_item(out, pad, i)?;
                }
            }
            Item::Group(g) => {
                for i in &g.items {
                    self.gen_trans_proto_group_item(out, pad, i)?;
                }
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
        writeln!(out)
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
        for args in self.tree.args.iter().filter(|i| !i.is_extern) {
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
        let mut set = HashSet::new();
        let mut first = true;
        for i in self.tree.fields.iter().filter_map(|i| i.func.as_deref()) {
            if !set.contains(i) {
                if first {
                    self.gen_comment(out, pad, "Extern functions")?;
                    first = false;
                }

                set.insert(i);
                write!(out, "{pad}fn {i}(&mut self, value: isize) -> isize")?;
                if self.stubs {
                    writeln!(out, " {{ todo!(\"{i}\") }}")?;
                } else {
                    writeln!(out, ";")?;
                }
            }
        }
        if !first {
            writeln!(out)?;
        }
        Ok(())
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
        if self.tree.fields.is_empty() {
            return Ok(());
        }
        self.gen_comment(out, pad, "Extract functions")?;
        for field in &self.tree.fields {
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
                write!(out, "{func}(insn, {pos}, {len}) as isize")?;
            }
        }
        Ok(())
    }

    fn gen_extract_set<W: Write>(
        &self,
        out: &mut W,
        pad: Pad,
        name: &str,
        items: &[SetValue<S>],
    ) -> io::Result<()> {
        writeln!(out, "{pad}let {name} = args_{name} {{")?;
        let p = pad.shift();
        for arg in items {
            write!(out, "{p}{}: ", arg.name)?;
            match arg.kind() {
                SetValueKind::Field(f) => self.gen_extract_field(out, f)?,
                SetValueKind::Const(v) => write!(out, "{v}")?,
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
        for arg in i.args.iter().filter(|i| self.gen.trans_check_arg(i.name())) {
            let name = arg.name();
            match arg.kind() {
                ValueKind::Set(set) => {
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

    fn gen_pattern_comment<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T, S>,
    ) -> io::Result<()> {
        for line in pattern.raw().lines() {
            writeln!(out, "{pad}// {line}")?;
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
        for (name, _) in self.gen.trans_args() {
            write!(out, ", {name}")?;
        }
        for arg in pattern
            .args
            .iter()
            .filter(|i| self.gen.trans_check_arg(i.name()))
        {
            write!(out, ", {}", arg.name())?;
        }
        writeln!(out, ") {{")?;
        self.gen.trans_success(out, pad.shift(), pattern)?;
        writeln!(out, "{}return true;", pad.shift())?;
        writeln!(out, "{pad}}}")?;
        Ok(())
    }

    fn gen_pattern_conditions<W: Write>(
        &self,
        out: &mut W,
        _: Pad,
        pat: &Pattern<T, S>,
    ) -> io::Result<()> {
        for (i, cond) in pat.cond.iter().enumerate() {
            if i != 0 {
                write!(out, " && ")?;
            }
            let inv = if cond.invert { "!" } else { "" };
            write!(out, "{inv}self.cond_{}()", cond.name)?;
        }
        Ok(())
    }

    fn gen_decode_overlap<W: Write>(
        &mut self,
        out: &mut W,
        mut pad: Pad,
        group: &Overlap<T, S>,
        prev: T,
    ) -> io::Result<()> {
        for i in &group.items {
            match i {
                OverlapItem::Pattern(pat) => {
                    self.gen_pattern_comment(out, pad, pat)?;
                    let mask = pat.mask.bit_andn(&prev);
                    let is_mask_non_zero = mask != T::zero();
                    let do_if = is_mask_non_zero || pat.has_conditions();
                    if do_if {
                        write!(out, "{pad}if ")?;
                        if pat.has_conditions() {
                            self.gen_pattern_conditions(out, pad, pat)?;
                        }
                        if is_mask_non_zero && pat.has_conditions() {
                            write!(out, " && ")?;
                        }
                        if is_mask_non_zero {
                            let opcode = pat.opcode.bit_andn(&prev);
                            write!(out, "insn & {mask:#x} == {opcode:#x}")?;
                        }
                        writeln!(out, " {{ ")?;
                        pad.right();
                    }

                    self.gen_call_trans_func(out, pad, pat)?;

                    if do_if {
                        pad.left();
                        writeln!(out, "{pad}}}")?;
                    }
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
        mut pad: Pad,
        group: &Group<T, S>,
        prev: T,
    ) -> io::Result<()> {
        let shared = group.shared_mask().bit_andn(&prev);

        writeln!(out, "{pad}match insn & {shared:#x} {{")?;
        pad.right();

        for item in &group.items {
            if let Item::Pattern(pattern) = item {
                self.gen_pattern_comment(out, pad, pattern)?;
            }

            write!(out, "{pad}{:#x}", item.opcode().bit_and(&shared))?;
            let exclusive = item.mask().bit_andn(&shared).bit_andn(&prev);
            if exclusive != T::zero() {
                let exclusive_opcode = item.opcode().bit_and(&exclusive);
                write!(out, " if insn & {exclusive:#x} == {exclusive_opcode:#x}")?;
            }
            writeln!(out, " => {{")?;
            pad.right();

            match item {
                Item::Pattern(pattern) => {
                    if pattern.has_conditions() {
                        write!(out, "{pad}if ")?;
                        self.gen_pattern_conditions(out, pad, pattern)?;
                        writeln!(out, " {{")?;
                        pad.right();
                    }
                    self.gen_call_trans_func(out, pad, pattern)?;
                    if pattern.has_conditions() {
                        pad.left();
                        writeln!(out, "{pad}}}")?;
                    }
                }
                Item::Overlap(overlap) => {
                    self.gen_decode_overlap(out, pad, overlap, *item.mask())?;
                }
                Item::Group(group) => {
                    self.gen_decode_group(out, pad, group, *item.mask())?;
                }
            }

            pad.left();
            writeln!(out, "{pad}}}")?;
        }

        writeln!(out, "{pad}_ => {{}}")?;

        pad.left();
        writeln!(out, "{pad}}}")?;

        Ok(())
    }

    fn gen_decode<W: Write>(&mut self, out: &mut W, mut pad: Pad) -> io::Result<()> {
        write!(out, "{pad}fn decode(&mut self, insn: {}", self.type_name)?;
        for (name, ty) in self.gen.trans_args() {
            write!(out, ", {name}: {ty}")?;
        }
        writeln!(out, ") -> bool {{")?;
        pad.right();
        self.gen_decode_group(out, pad, &self.tree.root, T::zero())?;
        writeln!(out)?;
        writeln!(out, "{pad}false")?;
        pad.left();
        writeln!(out, "{pad}}}")
    }

    fn gen_trait<W: Write>(&mut self, out: &mut W, mut pad: Pad) -> io::Result<()> {
        for attr in self.gen.trait_attrs() {
            writeln!(out, "{pad}{attr}")?;
        }
        if self.stubs {
            writeln!(out, "#[allow(unused_variables)]")?;
        }
        writeln!(out, "#[allow(clippy::unnecessary_cast)]")?;
        writeln!(out, "#[allow(clippy::collapsible_if)]")?;
        writeln!(out, "{pad}pub trait {}: Sized {{", self.trait_name)?;
        pad.right();
        self.gen_extern_func_proto(out, pad)?;
        self.gen_extract_fields(out, pad)?;
        if !self.tree.root.as_slice().len() != 0 {
            self.gen_comment(out, pad, "Translation functions")?;
            self.gen_trans_proto_group(out, pad, &self.tree.root)?;
            self.gen_cond_proto(out, pad)?;
            self.gen_comment(out, pad, "Decode function")?;
            self.gen_decode(out, pad)?;
            writeln!(out)?;
        }
        self.gen_comment(out, pad, "Generated user code for trait")?;
        self.gen.trait_body(out, pad)?;
        pad.left();
        writeln!(out, "{pad}}}")
    }

    /// Generate code.
    pub fn generate<W: Write>(&mut self, mut out: W) -> io::Result<()> {
        let pad = Pad(0);
        let out = &mut out;
        self.gen_args(out, pad)?;
        self.gen_trait(out, pad)?;
        writeln!(out)?;
        self.gen_comment(out, pad, "Generated user code")?;
        self.gen.end(out, pad, &self.opcodes)
    }
}
