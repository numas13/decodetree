//! Code generator for decodetree.

use std::{
    cmp,
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
    io::{self, Write},
    ops::Deref,
    rc::Rc,
};

use crate::{
    DecodeTree, Field, FieldDef, FieldItem, Group, Insn, Item, Overlap, OverlapItem, Pattern,
    SetValue, SetValueKind, Str, ValueKind,
};

type FieldUsageInfo<S = Str> = HashMap<S, (usize, Rc<FieldDef<S>>)>;

trait FieldUsage<S = Str> {
    /// Collect field usage statistics.
    ///
    /// Returns the number of processed patterns.
    fn field_usage(&self, output: &mut FieldUsageInfo<S>) -> usize;
}

impl<S> FieldUsage<S> for Rc<FieldDef<S>>
where
    S: Clone + Eq + Hash,
{
    fn field_usage(&self, output: &mut FieldUsageInfo<S>) -> usize {
        for item in self.items() {
            if let FieldItem::FieldRef(field_ref) = item {
                field_ref.field.field_usage(output);
            }
        }

        match output.get_mut(&self.name) {
            Some(entry) => {
                entry.0 += 1;
            }
            None => {
                output.insert(self.name.clone(), (1, self.clone()));
            }
        }

        0
    }
}

impl<S> FieldUsage<S> for Field<S>
where
    S: Clone + Eq + Hash,
{
    fn field_usage(&self, output: &mut FieldUsageInfo<S>) -> usize {
        if let Self::FieldRef(def) = self {
            for item in def.items() {
                if let FieldItem::FieldRef(field_ref) = item {
                    field_ref.field.field_usage(output);
                }
            }

            match output.get_mut(&def.name) {
                Some(entry) => {
                    entry.0 += 1;
                }
                None => {
                    output.insert(def.name.clone(), (1, def.clone()));
                }
            }
        }

        0
    }
}

impl<I, S> FieldUsage<S> for Pattern<I, S>
where
    S: Clone + Eq + Hash,
{
    fn field_usage(&self, output: &mut FieldUsageInfo<S>) -> usize {
        for i in &self.args {
            match i.kind() {
                ValueKind::Set(values) => {
                    for i in values {
                        if let SetValueKind::Field(field) = i.kind() {
                            field.field_usage(output);
                        }
                    }
                }
                ValueKind::Field(field) => {
                    field.field_usage(output);
                }
                _ => {}
            }
        }
        1
    }
}

impl<I, S> FieldUsage<S> for Overlap<I, S>
where
    S: Clone + Eq + Hash,
{
    fn field_usage(&self, output: &mut HashMap<S, (usize, Rc<FieldDef<S>>)>) -> usize {
        let mut count = 0;
        for item in &self.items {
            count += match item {
                OverlapItem::Pattern(i) => i.field_usage(output),
                OverlapItem::Group(i) => i.field_usage(output),
            };
        }
        count
    }
}

impl<I, S> FieldUsage<S> for Group<I, S>
where
    S: Clone + Eq + Hash,
{
    fn field_usage(&self, output: &mut FieldUsageInfo<S>) -> usize {
        let mut count = 0;
        for item in &self.items {
            count += match item {
                Item::Pattern(i) => i.field_usage(output),
                Item::Overlap(i) => i.field_usage(output),
                Item::Group(i) => i.field_usage(output),
            };
        }
        count
    }
}

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
    /// Additional attributes for argument sets.
    fn sets_attrs(&self) -> &[&str] {
        &[]
    }

    /// Additional attributes for trait.
    fn trait_attrs(&self) -> &[&str] {
        &[]
    }

    /// Additional parent traits.
    fn trait_parents(&self) -> &[&str] {
        &[]
    }

    /// Use to generate code inside trait.
    fn trait_body<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        Ok(())
    }

    /// Additional arguments to pass to decode function.
    fn decode_args(&self) -> &[(&str, &str)] {
        &[]
    }

    /// Additional attributes for condition functions.
    #[allow(unused_variables)]
    fn cond_attrs(&self, name: &str) -> &[&str] {
        &[]
    }

    /// Additional arguments for condition functions.
    #[allow(unused_variables)]
    fn cond_args(&self, name: &str) -> &[(&str, &str)] {
        &[]
    }

    /// Filter wich trans function prototypes should be generated.
    #[allow(unused_variables)]
    fn trans_proto_check(&self, name: &str) -> bool {
        true
    }

    /// Filter wich trans function calls should be generated.
    #[allow(unused_variables)]
    fn trans_call_check(&self, name: &str) -> bool {
        self.trans_proto_check(name)
    }

    /// Additional attributes for trans functions.
    #[allow(unused_variables)]
    fn trans_attrs(&self, name: &str) -> &[&str] {
        &[]
    }

    /// Filter arguments to trans functions.
    #[allow(unused_variables)]
    fn trans_check_arg(&self, name: &str, arg: &str) -> bool {
        true
    }

    /// Additional arguments to pass to trans functions.
    #[allow(unused_variables)]
    fn trans_args(&self, name: &str) -> &[(&str, &str)] {
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
    insn_type: Option<Str>,
    value_type: Option<Str>,
    zextract: Str,
    sextract: Str,
    stubs: bool,
    variable_size: bool,
    args_by_ref: bool,
    preload_field_max: u32,
    preload_field_usage: f64,
}

impl Default for GeneratorBuilder {
    fn default() -> Self {
        Self {
            trait_name: Str::from("Decode"),
            insn_type: None,
            value_type: None,
            zextract: Str::from("zextract"),
            sextract: Str::from("sextract"),
            stubs: false,
            variable_size: false,
            args_by_ref: false,
            preload_field_max: 5,
            preload_field_usage: 33.0,
        }
    }
}

impl GeneratorBuilder {
    /// Override Decode trait name.
    pub fn trait_name(mut self, name: &str) -> Self {
        self.trait_name = name.into();
        self
    }

    /// Override instruction type.
    pub fn insn_type(mut self, ty: &str) -> Self {
        self.insn_type = Some(ty.into());
        self
    }

    /// Override type for pattern values.
    pub fn value_type(mut self, ty: &str) -> Self {
        self.value_type = Some(ty.into());
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

    /// Generate instruction size checks.
    pub fn variable_size(mut self, variable_size: bool) -> Self {
        self.variable_size = variable_size;
        self
    }

    /// Pass `args_name` to trans function by reference.
    pub fn args_by_ref(mut self, by_ref: bool) -> Self {
        self.args_by_ref = by_ref;
        self
    }

    /// Set how many fields can be preloaded at once.
    ///
    /// Set `0` to disable fields preloading.
    pub fn preload_field_max(mut self, count: u32) -> Self {
        self.preload_field_max = count;
        self
    }

    /// Only fields used by `usage`% patterns will be preloaded.
    ///
    /// Set `0.0` to disable this filter.
    pub fn preload_field_usage(mut self, usage: f64) -> Self {
        self.preload_field_usage = usage;
        self
    }

    /// Build the `Generator`.
    pub fn build<T, S, G>(self, tree: &DecodeTree<T, S>, gen: G) -> Generator<T, S, G>
    where
        T: Insn,
        G: Gen<T, S>,
    {
        let insn_type = self
            .insn_type
            .unwrap_or_else(|| format!("u{}", T::width()).into());
        let value_type = self
            .value_type
            .unwrap_or_else(|| format!("i{}", T::width()).into());

        Generator {
            trait_name: self.trait_name,
            insn_type,
            value_type,
            zextract: self.zextract,
            sextract: self.sextract,
            stubs: self.stubs,
            variable_size: self.variable_size,
            args_by_ref: self.args_by_ref,
            preload_field_max: self.preload_field_max,
            preload_field_usage: self.preload_field_usage,
            gen,
            tree,
            opcodes: Default::default(),
            conditions: Default::default(),
        }
    }
}

#[derive(Clone)]
struct Scope<T, S> {
    // The bitmask checked in a parent scope.
    mask: T,
    // The instruction size checked in a parent scope.
    size: u32,
    variable_size: bool,

    preload_field_max: u32,
    preload_field_usage: f64,
    preloaded_fields: HashMap<S, Rc<FieldDef<S>>>,
}

impl<T, S> Scope<T, S>
where
    T: Insn,
    S: Clone + Eq + Hash + fmt::Display,
{
    fn new<G>(generator: &Generator<T, S, G>) -> Self {
        Self {
            mask: T::zero(),
            size: 0,
            variable_size: generator.variable_size,
            preload_field_max: generator.preload_field_max,
            preload_field_usage: generator.preload_field_usage,
            preloaded_fields: Default::default(),
        }
    }

    fn child(&self, mask: T, size: u32) -> Self {
        Self {
            mask,
            size,
            variable_size: self.variable_size,
            preload_field_max: self.preload_field_max,
            preload_field_usage: self.preload_field_usage,
            preloaded_fields: self.preloaded_fields.clone(),
        }
    }

    fn preload_fields(
        &mut self,
        out: &mut impl Write,
        pad: Pad,
        item: &dyn FieldUsage<S>,
    ) -> io::Result<()> {
        let mut fields = Default::default();
        let pattern_count = item.field_usage(&mut fields) as f64;
        let mut fields: Vec<_> = fields.into_values().collect();
        fields.sort_by(|(a, _), (b, _)| b.cmp(a));

        // writeln!(out, "{pad}// Fields usage statistics:")?;
        // writeln!(out, "{pad}// ")?;
        // writeln!(out, "{pad}//  count | patterns | field")?;
        // writeln!(out, "{pad}//--------|----------|-------------")?;
        // for (count, field) in &fields {
        //     let usage = *count as f64 / pattern_count * 100.0;
        //     writeln!(out, "{pad}//  {count:>5} | {usage:7.1}% | {}", field.name())?;
        // }
        // writeln!(out)?;

        let mut n = 0;
        for (count, field) in &fields {
            let usage = *count as f64 / pattern_count * 100.0;
            if usage < self.preload_field_usage {
                break;
            }
            let name = field.name();
            if self.variable_size && self.size < field.required_size() {
                // required instruction size is not checked yet
                continue;
            }
            if self.is_preloaded_field(name) {
                // already preloaded
                continue;
            }
            n += 1;
            if n > self.preload_field_max {
                // no more preloads
                break;
            }
            writeln!(out, "{pad}// {count:} uses in {usage:.1}% patterns")?;
            writeln!(out, "{pad}let {name} = self.extract_{name}(insn);")?;
            self.preloaded_fields.insert(name.clone(), field.clone());
        }

        if n != 0 {
            writeln!(out)?;
        }

        Ok(())
    }

    fn is_preloaded_field(&self, name: &S) -> bool {
        self.preloaded_fields.contains_key(name)
    }
}

/// Decodetree generator.
pub struct Generator<'a, T = super::DefaultInsn, S = Str, G = ()> {
    trait_name: Str,
    insn_type: Str,
    value_type: Str,
    zextract: Str,
    sextract: Str,
    stubs: bool,
    variable_size: bool,
    args_by_ref: bool,
    preload_field_max: u32,
    preload_field_usage: f64,
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
    S: Clone + Eq + Hash + fmt::Display + Deref<Target = str>,
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
        for i in &pattern.cond {
            self.conditions.insert(&i.name);
        }

        if self.opcodes.contains(&*pattern.name) {
            return Ok(());
        }

        self.opcodes.insert(&pattern.name);

        if !self.gen.trans_proto_check(pattern.name()) {
            return Ok(());
        }

        write!(out, "{pad}fn trans_{}(&mut self", pattern.name())?;
        for (name, ty) in self.gen.trans_args(pattern.name()) {
            write!(out, ", {name}: {ty}")?;
        }
        for value in pattern
            .args
            .iter()
            .filter(|i| self.gen.trans_check_arg(pattern.name(), i.name().as_ref()))
        {
            let name = value.name();
            write!(out, ", {name}: ")?;
            match value.kind() {
                ValueKind::Set(..) => write!(out, "args_{name}")?,
                _ => write!(out, "{}", self.value_type)?,
            }
        }
        write!(out, ") -> Result<bool, Self::Error>")?;

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
            for attr in self.gen.cond_attrs(i) {
                writeln!(out, "{attr}")?;
            }
            write!(out, "{pad}fn cond_{i}(&self")?;
            for (arg, ty) in self.gen.cond_args(i) {
                write!(out, ", {arg}: {ty}")?;
            }
            writeln!(out, ") -> bool;")?;
        }
        writeln!(out)
    }

    fn gen_args<W: Write>(&self, out: &mut W, pad: Pad) -> io::Result<()> {
        self.gen_comment(out, pad, "Argument sets")?;
        for args in self.tree.sets.iter().filter(|i| !i.is_extern) {
            for attr in self.gen.sets_attrs() {
                writeln!(out, "{attr}")?;
            }
            writeln!(out, "{pad}#[allow(non_camel_case_types)]")?;
            if args.items.is_empty() {
                writeln!(out, "{pad}pub struct args_{};", args.name)?;
            } else {
                writeln!(out, "{pad}pub struct args_{} {{", args.name)?;
                for i in &args.items {
                    let ty = i.ty.as_deref().unwrap_or(&self.value_type);
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
                let ty = &self.value_type;
                write!(out, "{pad}fn {i}(&self, value: {ty}) -> {ty}")?;
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
        let ty = &self.value_type;
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
                    write!(out, "{func}(insn, {pos}, {len}) as {ty}")?;
                }
                FieldItem::FieldRef(f) => {
                    let (field, len) = (f.field(), f.len());
                    let func = if f.sxt() {
                        &self.sextract
                    } else {
                        &self.zextract
                    };
                    let name = field.name();
                    let insn_type = &self.insn_type;
                    write!(
                        out,
                        "{func}(self.extract_{name}(insn) as {insn_type}, 0, {len}) as {ty}"
                    )?;
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
                        writeln!(out, "{pad}out |= {func}(insn, {pos}, {len}) as {ty};")?;
                    }
                    FieldItem::FieldRef(f) => {
                        let (field, len) = (f.field(), f.len());
                        writeln!(out, "{pad}out <<= {};", len)?;
                        let s = ["", "s"][f.sxt() as usize];
                        let name = field.name();
                        writeln!(out, "{pad}let tmp0 = Self::extract_{name}(insn);")?;
                        writeln!(out, "{pad}out |= {s}extract(tmp0, 0, {len}) as {ty};")?;
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
        let insn_ty = &self.insn_type;
        let val_ty = &self.value_type;
        for field in &self.tree.fields {
            let name = field.name();
            writeln!(
                out,
                "{pad}fn extract_{name}(&self, insn: {insn_ty}) -> {val_ty} {{",
            )?;
            self.gen_extract_field_body(out, field, pad.shift())?;
            writeln!(out, "{pad}}}",)?;
            writeln!(out)?;
        }
        Ok(())
    }

    fn gen_extract_field<W: Write>(
        &self,
        out: &mut W,
        scope: &Scope<T, S>,
        arg: Option<&S>,
        sep: char,
        field: &Field<S>,
    ) -> io::Result<()> {
        match field {
            Field::FieldRef(field) => {
                let name = field.name();
                if !scope.is_preloaded_field(name) {
                    write!(out, "{sep} self.extract_{name}(insn)")?;
                } else if arg.map(|arg| arg != name).unwrap_or(true) {
                    write!(out, "{sep} {name}")?;
                }
            }
            Field::Field(field) => {
                let pos = field.pos();
                let len = field.len();
                let func = if field.sxt() {
                    &self.sextract
                } else {
                    &self.zextract
                };
                write!(
                    out,
                    "{sep} {func}(insn, {pos}, {len}) as {}",
                    self.value_type
                )?;
            }
        }
        Ok(())
    }

    fn gen_extract_set<W: Write>(
        &self,
        out: &mut W,
        pad: Pad,
        scope: &Scope<T, S>,
        name: &str,
        items: &[SetValue<S>],
    ) -> io::Result<()> {
        writeln!(out, "{pad}let {name} = args_{name} {{")?;
        let p = pad.shift();
        for arg in items {
            write!(out, "{p}{}", arg.name())?;
            match arg.kind() {
                SetValueKind::Field(f) => {
                    self.gen_extract_field(out, scope, Some(arg.name()), ':', f)?
                }
                SetValueKind::Const(v) => write!(out, ": {v}")?,
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
        pattern: &Pattern<T, S>,
        pad: Pad,
        scope: &Scope<T, S>,
    ) -> io::Result<()> {
        for arg in pattern
            .args
            .iter()
            .filter(|i| self.gen.trans_check_arg(pattern.name(), i.name()))
        {
            let name = arg.name();
            match arg.kind() {
                ValueKind::Set(set) => {
                    self.gen_extract_set(out, pad, scope, name, set.as_slice())?;
                }
                ValueKind::Field(f) => match f {
                    Field::FieldRef(def)
                        if scope.is_preloaded_field(def.name()) && name == def.name() => {}
                    _ => {
                        write!(out, "{pad}let {name} ")?;
                        self.gen_extract_field(out, scope, None, '=', f)?;
                        writeln!(out, ";")?;
                    }
                },
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
        mut pad: Pad,
        pattern: &Pattern<T, S>,
        scope: &Scope<T, S>,
    ) -> io::Result<()> {
        self.gen_extract_args(out, pattern, pad, scope)?;

        let gen_call = self.gen.trans_call_check(pattern.name());
        if gen_call {
            write!(out, "{pad}if Self::trans_{}(self", pattern.name)?;
            for (name, _) in self.gen.trans_args(pattern.name()) {
                write!(out, ", {name}")?;
            }
            for arg in pattern
                .args
                .iter()
                .filter(|i| self.gen.trans_check_arg(pattern.name(), i.name()))
            {
                if self.args_by_ref && arg.is_set() {
                    write!(out, ", &{}", arg.name())?;
                } else {
                    write!(out, ", {}", arg.name())?;
                }
            }
            writeln!(out, ")? {{")?;
            pad.right();
        }

        self.gen.trans_success(out, pad, pattern)?;

        writeln!(out, "{pad}return Ok({});", pattern.size())?;

        if gen_call {
            pad.left();
            writeln!(out, "{pad}}}")?;
        }
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
            write!(out, "{inv}self.cond_{}(", cond.name())?;
            for (i, (arg, _)) in self.gen.cond_args(cond.name()).iter().enumerate() {
                if i != 0 {
                    write!(out, ", ")?;
                }
                write!(out, "{arg}")?;
            }
            write!(out, ")")?;
        }
        Ok(())
    }

    fn gen_decode_overlap<W: Write>(
        &mut self,
        out: &mut W,
        mut pad: Pad,
        group: &Overlap<T, S>,
        mut scope: Scope<T, S>,
    ) -> io::Result<()> {
        scope.preload_fields(out, pad, group)?;

        for i in &group.items {
            match i {
                OverlapItem::Pattern(pat) => {
                    let mask = pat.mask.bit_andn(&scope.mask);

                    scope.size = self.gen_check_insn_size(out, pad, scope.size, mask.size())?;

                    self.gen_pattern_comment(out, pad, pat)?;
                    let is_mask_non_zero = mask != T::zero();
                    if is_mask_non_zero || pat.has_conditions() {
                        write!(out, "{pad}if ")?;
                        if pat.has_conditions() {
                            self.gen_pattern_conditions(out, pad, pat)?;
                        }
                        if is_mask_non_zero && pat.has_conditions() {
                            write!(out, " && ")?;
                        }
                        if is_mask_non_zero {
                            let opcode = pat.opcode.bit_andn(&scope.mask);
                            write!(out, "insn & {mask:#x} == {opcode:#x}")?;
                        }
                        write!(out, " ")?;
                    } else {
                        write!(out, "{pad}")?;
                    }

                    writeln!(out, "{{ ")?;
                    pad.right();
                    self.gen_check_insn_size(out, pad, scope.size, pat.size())?;
                    self.gen_call_trans_func(out, pad, pat, &scope)?;
                    pad.left();
                    writeln!(out, "{pad}}}")?;
                }
                OverlapItem::Group(group) => {
                    self.gen_decode_group(out, pad, group, scope.clone())?;
                }
            }
        }
        Ok(())
    }

    fn gen_check_insn_size(
        &self,
        out: &mut impl Write,
        pad: Pad,
        old_size: u32,
        size: u32,
    ) -> io::Result<u32> {
        if self.variable_size && old_size < size {
            writeln!(out, "{pad}if insn_size < {size} {{")?;
            writeln!(out, "{}return Err(self.need_more({size}));", pad.shift())?;
            writeln!(out, "{pad}}}")?;
            Ok(size)
        } else {
            Ok(old_size)
        }
    }

    fn gen_decode_group<W: Write>(
        &mut self,
        out: &mut W,
        mut pad: Pad,
        group: &Group<T, S>,
        mut scope: Scope<T, S>,
    ) -> io::Result<()> {
        let shared = group.shared_mask().bit_andn(&scope.mask);
        let shared_size = shared.size();

        scope.size = self.gen_check_insn_size(out, pad, scope.size, shared_size)?;

        scope.preload_fields(out, pad, group)?;

        writeln!(out, "{pad}match insn & {shared:#x} {{")?;
        pad.right();

        let mut item_sizes = HashMap::<T, u32>::new();
        if self.variable_size {
            for item in &group.items {
                let opcode = item.opcode().bit_and(&shared);
                let item_size = match item {
                    Item::Pattern(pattern) => pattern.size(),
                    _ => item.mask().size(),
                };
                if shared_size < item_size {
                    let e = item_sizes.entry(opcode).or_default();
                    *e = cmp::max(*e, item_size);
                }
            }
        }

        for item in &group.items {
            if let Item::Pattern(pattern) = item {
                self.gen_pattern_comment(out, pad, pattern)?;
            }

            let opcode = item.opcode().bit_and(&shared);

            let mut item_size = scope.size;
            if self.variable_size {
                if let Some(size) = item_sizes.remove(&opcode) {
                    if item_size < size {
                        write!(out, "{pad}{opcode:#x} if insn_size < {size} => ")?;
                        writeln!(out, "return Err(self.need_more({size})),")?;
                        item_size = size;
                    }
                }
            }

            write!(out, "{pad}{opcode:#x}")?;
            let exclusive = item.mask().bit_andn(&shared).bit_andn(&scope.mask);
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
                    self.gen_call_trans_func(out, pad, pattern, &scope)?;
                    if pattern.has_conditions() {
                        pad.left();
                        writeln!(out, "{pad}}}")?;
                    }
                }
                Item::Overlap(overlap) => {
                    let scope = scope.child(*item.mask(), item_size);
                    self.gen_decode_overlap(out, pad, overlap, scope)?;
                }
                Item::Group(group) => {
                    let scope = scope.child(*item.mask(), item_size);
                    self.gen_decode_group(out, pad, group, scope)?;
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
        self.gen_comment(out, pad, "Decode function")?;
        write!(out, "{pad}fn decode(&mut self, insn: {}", self.insn_type)?;
        if self.variable_size {
            write!(out, ", insn_size: usize")?;
        }
        for (name, ty) in self.gen.decode_args() {
            write!(out, ", {name}: {ty}")?;
        }
        writeln!(out, ") -> Result<usize, Self::Error> {{")?;
        pad.right();
        self.gen_decode_group(out, pad, &self.tree.root, Scope::new(self))?;
        writeln!(out)?;
        writeln!(out, "{pad}Err(self.fail())")?;
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
        write!(out, "{pad}pub trait {}: Sized", self.trait_name)?;
        for parent in self.gen.trait_parents() {
            write!(out, " + {parent}")?;
        }
        writeln!(out, " {{")?;
        pad.right();
        writeln!(out, "{pad}type Error;")?;
        writeln!(out)?;
        if self.variable_size {
            writeln!(out, "{pad}fn need_more(&self, size: usize) -> Self::Error;")?;
        }
        writeln!(out, "{pad}fn fail(&self) -> Self::Error;")?;
        self.gen_extern_func_proto(out, pad)?;
        self.gen_extract_fields(out, pad)?;
        if !self.tree.root.as_slice().len() != 0 {
            self.gen_comment(out, pad, "Translation functions")?;
            self.gen_trans_proto_group(out, pad, &self.tree.root)?;
            self.gen_cond_proto(out, pad)?;
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
