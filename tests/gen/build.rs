use std::{
    collections::HashSet,
    fs::{self, File},
    io::{self, BufWriter, Write},
    path::Path,
};

use decodetree::{
    gen::{Gen, Generator, Pad},
    Pattern,
};

#[derive(Default)]
struct Helper {
    opcodes: bool,
}

impl<T> Gen<T, &'_ str> for Helper {
    fn trans_body<W: Write>(
        &mut self,
        out: &mut W,
        mut pad: Pad,
        pattern: &Pattern<T, &str>,
    ) -> io::Result<bool> {
        let opcode = pattern.name().to_uppercase();
        writeln!(out, "{{")?;
        pad.right();
        writeln!(out, "{pad}self.set_opcode(Opcode::{opcode});")?;
        writeln!(out, "{pad}true")?;
        pad.left();
        writeln!(out, "{pad}}}")?;
        Ok(true)
    }

    fn trait_body<W: Write>(&mut self, out: &mut W, pad: Pad) -> io::Result<()> {
        writeln!(out)?;
        writeln!(out, "{pad}fn set_opcode(&mut self, opcode: Opcode);")?;
        Ok(())
    }

    fn end<W: Write>(
        &mut self,
        out: &mut W,
        mut pad: Pad,
        opcodes: &HashSet<&str>,
    ) -> io::Result<()> {
        if self.opcodes {
            let opcodes = {
                let mut vec: Vec<_> = opcodes.iter().collect();
                vec.sort();
                vec
            };

            writeln!(out)?;
            writeln!(out, "{pad}#[derive(Copy, Clone, Debug, PartialEq, Eq)]")?;
            writeln!(out, "{pad}pub enum Opcode {{")?;
            pad.right();
            for i in opcodes {
                writeln!(out, "{pad}{},", i.to_uppercase())?;
            }
            pad.left();
            writeln!(out, "{pad}}}")?;
        }
        Ok(())
    }
}

fn gen<T>(trait_name: &str, path: &str, out: &str, opt: bool)
where
    T: Default + std::hash::Hash + std::fmt::LowerHex + Ord + decodetree::Insn,
{
    println!("cargo:rerun-if-changed={path}");
    let src = fs::read_to_string(path).unwrap();
    let mut tree = match decodetree::from_str::<T, &str>(&src) {
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

    if opt {
        tree.optimize();
    }

    Generator::builder()
        .trait_name(trait_name)
        .stubs(true)
        .build(&tree, Helper { opcodes: !opt })
        .generate(&mut out)
        .unwrap();
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    gen::<u32>(
        "Decode",
        "src/insn32.decode",
        &format!("{out_dir}/generated.rs"),
        false,
    );

    gen::<u32>(
        "Decode",
        "src/insn32.decode",
        &format!("{out_dir}/generated_opt.rs"),
        true,
    );
}
