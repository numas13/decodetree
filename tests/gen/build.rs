use std::{
    collections::HashSet,
    fs::{self, File},
    io::{self, BufWriter, Write},
    path::Path,
};

use decodetree::{
    generator::{Gen, Pad},
    Parser, Pattern,
};

#[derive(Default)]
struct Helper {
    opcodes: bool,
}

impl<T> Gen<T, &'_ str> for Helper {
    fn trans_args(&self, _: &str) -> &[(&str, &str)] {
        &[("insn", "u32")]
    }

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
        writeln!(out, "{pad}Ok(true)")?;
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
            writeln!(out, "#[allow(non_camel_case_types)]")?;
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

struct Generate<'a> {
    path: &'a str,
    trait_name: &'a str,
    sizes: &'a [u32],
    optimize: bool,
    stubs: bool,
    variable_size: bool,
}

impl<'a> Generate<'a> {
    fn new(path: &'a str) -> Self {
        Self {
            path,
            trait_name: "Decode",
            sizes: &[32],
            optimize: false,
            stubs: true,
            variable_size: false,
        }
    }

    fn gen<T>(&self, out: &str)
    where
        T: Default + std::hash::Hash + std::fmt::LowerHex + Ord + decodetree::Insn,
    {
        println!("cargo:rerun-if-changed={}", self.path);

        let src = fs::read_to_string(self.path).unwrap();
        let parser = Parser::<T, &str>::new(&src).set_insn_size(self.sizes);

        let mut tree = match parser.parse() {
            Ok(tree) => tree,
            Err(errors) => {
                for err in errors.iter(self.path) {
                    eprintln!("{err}");
                }
                std::process::exit(1);
            }
        };

        if self.optimize {
            tree.optimize();
        }

        if let Some(parent) = Path::new(out).parent() {
            fs::create_dir_all(parent).unwrap();
        }
        let mut out = BufWriter::new(File::create(out).unwrap());

        decodetree::Generator::builder()
            .trait_name(self.trait_name)
            .stubs(self.stubs)
            .variable_size(self.variable_size)
            .build(
                &tree,
                Helper {
                    opcodes: !self.optimize,
                },
            )
            .generate(&mut out)
            .unwrap();
    }
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    let mut gen = Generate::new("src/insn.decode");
    gen.gen::<u32>(&format!("{out_dir}/generated.rs"));
    gen.optimize = true;
    gen.gen::<u32>(&format!("{out_dir}/generated_opt.rs"));

    let mut gen = Generate::new("src/insn_vs.decode");
    gen.sizes = &[8, 16, 24, 32];
    gen.variable_size = true;
    gen.gen::<u32>(&format!("{out_dir}/generated_vs.rs"));
    gen.optimize = true;
    gen.gen::<u32>(&format!("{out_dir}/generated_vs_opt.rs"));

    let gen = Generate::new("src/extract.decode");
    gen.gen::<u32>(&format!("{out_dir}/generated_extract.rs"));
}
