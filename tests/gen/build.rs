use std::{
    fs::{self, File},
    io::{self, BufWriter, Write},
    path::Path,
};

use decodetree::{
    gen::{Generator, Pad, Visitor},
    Pattern,
};

#[derive(Default)]
struct Helper {}

impl<T> Visitor<T> for Helper {
    fn visit_trans_proto_pattern<W: Write>(
        &mut self,
        out: &mut W,
        pad: Pad,
        pattern: &Pattern<T>,
    ) -> io::Result<bool> {
        writeln!(
            out,
            "{pad}self.set_opcode(Opcode::{});",
            pattern.name.to_uppercase()
        )?;
        writeln!(out, "{pad}true")?;
        Ok(true)
    }

    #[allow(unused_variables)]
    #[inline]
    fn visit_trait_body<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        writeln!(out)?;
        writeln!(out, "{pad}fn set_opcode(&mut self, opcode: Opcode);")?;
        Ok(())
    }
}

fn gen<T>(trait_name: &str, path: &str, out: &str)
where
    T: Default + std::hash::Hash + std::fmt::LowerHex + Ord + decodetree::Insn,
{
    println!("cargo:rerun-if-changed={path}");
    let src = fs::read_to_string(path).unwrap();
    let tree = match decodetree::parse::<T>(&src) {
        Ok(tree) => tree,
        Err(errors) => {
            for err in errors.iter(path) {
                eprintln!("{err}");
            }
            std::process::exit(1);
        }
    };
    if let Some(parent) = Path::new(out).parent() {
        fs::create_dir_all(parent).unwrap();
    }
    let mut out = BufWriter::new(File::create(out).unwrap());

    Generator::<T, Helper>::builder()
        .trait_name(trait_name)
        .stubs(true)
        .opcodes(true)
        .visitor(Helper::default())
        .build(&tree)
        .gen(&mut out)
        .unwrap();
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    gen::<u32>(
        "Decode",
        "src/insn32.decode",
        &format!("{out_dir}/decode32.rs"),
    );
}
