pub(crate) mod parser;

#[cfg(feature = "gen")]
mod gen;

pub use parser::ErrorPrinter;
pub use parser::Errors;

#[cfg(feature = "gen")]
pub use gen::Generator;

use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

pub trait Insn: Sized {
    fn width() -> u32;
    fn zero() -> Self;
    fn ones() -> Self;
    fn set_bit(&mut self, offset: u32);
    fn and(&mut self, other: &Self);
    fn or(&mut self, other: &Self);
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

            fn set_bit(&mut self, offset: u32) {
                *self |= 1 << offset;
            }

            fn and(&mut self, other: &Self) {
                *self &= *other;
            }

            fn or(&mut self, other: &Self) {
                *self |= *other;
            }
        })+
    );
}

impl_insn!(u8, u16, u32, u64, u128);

#[derive(Clone, Debug)]
pub enum FieldItem {
    Field {
        pos: u32,
        len: u32,
        sxt: bool,
    },
    FieldRef {
        field: Rc<Field>,
        len: u32,
        sxt: bool,
    },
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub func: Option<String>,
    pub items: Vec<FieldItem>,
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub name: String,
    pub ty: Option<String>,
    pub value: Option<ValueKind>,
}

impl From<&parser::Arg<'_>> for Arg {
    fn from(value: &parser::Arg<'_>) -> Self {
        Self {
            name: value.name().to_owned(),
            ty: value.ty().map(str::to_owned),
            value: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Args {
    pub name: String,
    pub is_extern: bool,
    pub items: Vec<Arg>,
}

impl From<&parser::ArgsDef<'_>> for Args {
    fn from(value: &parser::ArgsDef<'_>) -> Self {
        Self {
            name: value.name.to_string(),
            is_extern: value.is_extern,
            items: value.args.iter().map(Arg::from).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueKind {
    Field(Rc<Field>),
    Const(i64),
}

#[derive(Clone, Debug)]
pub struct Value {
    pub name: String,
    pub kind: ValueKind,
}

impl Value {
    fn new_field(name: String, field: Rc<Field>) -> Self {
        Self {
            name,
            kind: ValueKind::Field(field),
        }
    }

    fn new_const(name: String, value: i64) -> Self {
        Self {
            name,
            kind: ValueKind::Const(value),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Pattern<T> {
    pub mask: T,
    pub opcode: T,
    pub name: String,
    pub sets: Vec<Args>,
    pub args: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct Group<T> {
    pub mask: T,
    pub opcode: T,
    pub overlap: bool,
    pub items: Vec<Item<T>>,
}

#[derive(Clone, Debug)]
pub enum Item<T> {
    Pattern(Pattern<T>),
    Group(Box<Group<T>>),
}

#[derive(Clone, Debug, Default)]
pub struct DecodeTree<T> {
    pub fields: HashMap<String, Rc<Field>>,
    pub args: HashMap<String, Args>,
    pub formats: HashMap<String, Rc<Pattern<T>>>,
    pub items: Vec<Item<T>>,
}

impl<T> DecodeTree<T>
where
    T: Insn,
{
    fn pattern_def(&mut self, def: &parser::PatternDef) -> Pattern<T> {
        use parser::PatternItem as E;

        let mut p = mem::size_of::<T>() as u32 * 8;
        let mut fm = T::zero();
        let mut fo = T::zero();
        let mut m = T::zero();
        let mut o = T::zero();
        let mut sets = vec![];
        let mut args = vec![];
        for i in def.items.iter() {
            match i {
                E::FixedBits(i) => {
                    for c in i.chars() {
                        p -= 1;
                        if c == '0' || c == '1' {
                            m.set_bit(p);
                        }
                        if c == '1' {
                            o.set_bit(p);
                        }
                    }
                }
                E::Field(i) => {
                    p -= i.len();
                    let field = Rc::new(Field {
                        name: String::new(),
                        func: None,
                        items: vec![FieldItem::Field {
                            pos: p,
                            len: i.len(),
                            sxt: i.sign_extend(),
                        }],
                    });
                    args.push(Value::new_field(i.name().to_string(), field));
                }
                E::ArgsRef(i) => {
                    let r = self.args.get(&i.to_string()).unwrap();
                    sets.push(r.clone());
                }
                E::FieldRef(i) => {
                    let r = self.fields.get(&i.field.to_string()).unwrap().clone();
                    args.push(Value::new_field(i.name.to_string(), r));
                }
                E::Const(i) => {
                    args.push(Value::new_const(i.name().to_string(), i.value()));
                }
                E::FormatRef(i) => {
                    let r = self.formats.get(&i.to_string()).unwrap().clone();
                    fm.or(&r.mask);
                    fo.or(&r.opcode);
                    sets.extend(r.sets.iter().cloned());
                    args.extend_from_slice(&r.args);
                }
            }
        }
        m.or(&fm);
        o.or(&fo);

        for set in &mut sets {
            for arg in set.items.iter_mut() {
                for i in 0..args.len() {
                    if args[i].name == arg.name {
                        arg.value = Some(args[i].kind.clone());
                        args.remove(i);
                        break;
                    }
                }
            }
        }

        Pattern {
            mask: m,
            opcode: o,
            name: def.name.to_string(),
            sets,
            args,
        }
    }

    fn group(&mut self, group: &parser::Group) -> Group<T> {
        use parser::GroupItem as E;

        let mut mask = T::ones();
        let mut opcode = T::zero();
        let mut items = vec![];
        for i in group.items.iter() {
            let e = match i {
                E::PatternDef(def) => {
                    let p = self.pattern_def(def);
                    mask.and(&p.mask);
                    opcode.or(&p.opcode);
                    Item::Pattern(p)
                }
                E::Group(group) => {
                    let g = self.group(group);
                    mask.and(&g.mask);
                    opcode.or(&g.opcode);
                    Item::Group(Box::new(g))
                }
            };
            items.push(e);
        }

        opcode.and(&mask);

        Group {
            mask,
            opcode,
            overlap: group.overlap,
            items,
        }
    }

    fn from(&mut self, data: &parser::Data) {
        for (k, v) in data.fields.iter() {
            let mut items = vec![];

            for f in &v.items {
                let e = match f {
                    parser::Field::Unnamed(f) => FieldItem::Field {
                        pos: f.pos(),
                        len: f.len(),
                        sxt: f.sign_extend(),
                    },
                    parser::Field::Named(f) => {
                        let field = self.fields.get(f.name()).unwrap().clone();
                        FieldItem::FieldRef {
                            field,
                            len: f.len(),
                            sxt: f.sign_extend(),
                        }
                    }
                };
                items.push(e);
            }

            self.fields.insert(
                k.to_string(),
                Rc::new(Field {
                    name: k.to_string(),
                    func: v.func.map(|i| i.to_string()),
                    items,
                }),
            );
        }

        for (k, v) in data.args.iter() {
            self.args.insert(k.to_string(), v.into());
        }

        for (k, v) in data.formats.iter() {
            use parser::FormatItem as E;

            let mut p = mem::size_of::<T>() as u32 * 8;
            let mut m = T::zero();
            let mut o = T::zero();
            let mut sets = vec![];
            let mut args = vec![];
            for i in v.items.iter() {
                match i {
                    E::FixedBits(i) => {
                        for c in i.chars() {
                            p -= 1;
                            if c == '0' || c == '1' {
                                m.set_bit(p);
                            }
                            if c == '1' {
                                o.set_bit(p);
                            }
                        }
                    }
                    E::Field(i) => {
                        p -= i.len();
                        let field = Rc::new(Field {
                            name: String::new(),
                            func: None,
                            items: vec![FieldItem::Field {
                                pos: p,
                                len: i.len(),
                                sxt: i.sign_extend(),
                            }],
                        });
                        args.push(Value::new_field(i.name().to_string(), field));
                    }
                    E::ArgsRef(i) => {
                        let a = self.args.get(&i.to_string()).unwrap();
                        sets.push(a.clone());
                    }
                    E::FieldRef(i) => {
                        let f = self.fields.get(&i.field.to_string()).unwrap().clone();
                        args.push(Value::new_field(i.name.to_string(), f));
                    }
                    E::Const(i) => {
                        args.push(Value::new_const(i.name().to_string(), i.value()));
                    }
                }
            }

            self.formats.insert(
                k.to_string(),
                Rc::new(Pattern {
                    name: k.to_string(),
                    sets,
                    args,
                    mask: m,
                    opcode: o,
                }),
            );
        }

        for i in data.items.iter() {
            match i {
                parser::Item::Pattern(def) => {
                    let pattern = self.pattern_def(def);
                    self.items.push(Item::Pattern(pattern));
                }
                parser::Item::Group(def) => {
                    let group = self.group(def);
                    self.items.push(Item::Group(Box::new(group)));
                }
            }
        }
    }
}

pub fn parse<T>(src: &str) -> Result<DecodeTree<T>, Errors>
where
    T: Insn + Default,
{
    let data = parser::parse(T::width(), src)?;
    let mut tree = DecodeTree::default();
    tree.from(&data);
    Ok(tree)
}
