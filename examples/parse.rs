use std::{env, fs, process};

use decodetree::{DecodeTree, Group, Overlap, Parser, Pattern};

fn align(depth: usize) {
    for _ in 0..depth {
        print!("  ");
    }
}

struct Dump {
    cli: Cli,
    tree: DecodeTree,
}

impl Dump {
    fn dump_field_len(&self, len: u32, sxt: bool) {
        print!("{}{len}", ["", "s"][sxt as usize]);
    }

    fn dump_unnamed_field(&self, field: &decodetree::UnnamedField) {
        print!("{}:", field.pos());
        self.dump_field_len(field.len(), field.sxt());
    }

    fn dump_named_field(&self, field: &decodetree::FieldRef) {
        print!("{}:", field.field().name());
        self.dump_field_len(field.len(), field.sxt());
    }

    fn dump_field_item(&self, item: &decodetree::FieldItem) {
        use decodetree::FieldItem as E;
        match item {
            E::Field(f) => self.dump_unnamed_field(f),
            E::FieldRef(f) => self.dump_named_field(f),
        }
    }

    fn dump_field(&self, field: &decodetree::Field) {
        use decodetree::Field as E;
        match field {
            E::Field(f) => self.dump_unnamed_field(f),
            E::FieldRef(f) => print!("%{}", f.name()),
        }
    }

    fn dump_fields(&self) {
        if self.tree.fields().is_empty() {
            return;
        }
        println!("# Fields:");
        for field in self.tree.fields() {
            print!("%{}", field.name());
            for item in field.items() {
                print!(" ");
                self.dump_field_item(item);
            }
            if let Some(func) = field.func() {
                print!(" !function={func}");
            }
            println!();
        }
        println!();
    }

    fn dump_args(&self) {
        if self.tree.sets().is_empty() {
            return;
        }
        println!("# Args:");
        for set in self.tree.sets() {
            print!("&{}", set.name());
            for arg in set.values() {
                print!(" {}", arg.name());
            }
            if set.is_extern() {
                print!(" !extern");
            }
            println!();
        }
        println!();
    }

    fn dump_set_value(&self, value: &decodetree::SetValue) {
        use decodetree::SetValueKind as E;
        print!("{}=", value.name());
        match value.kind() {
            E::Field(field) => self.dump_field(field),
            E::Const(value) => print!("{value}"),
        }
    }

    fn dump_value(&self, value: &decodetree::Value) {
        use decodetree::ValueKind as E;
        let name = value.name();
        match value.kind() {
            E::Set(set) => {
                print!("&{name}");
                for arg in set {
                    print!(" ");
                    self.dump_set_value(arg);
                }
            }
            E::Field(field) => {
                print!("{name}=");
                self.dump_field(field);
            }
            E::Const(value) => {
                print!("{name}={value}");
            }
        }
    }

    fn dump_pattern(&self, pat: &Pattern, depth: usize) {
        #[cfg(feature = "raw")]
        if self.cli.raw {
            for line in pat.raw().lines() {
                align(depth);
                println!("# {line}");
            }
        }
        align(depth);
        print!("{:04x}:{:04x}:{:<2} ", pat.opcode(), pat.mask(), pat.size());
        print!("{}", pat.name());
        for arg in pat.values() {
            print!(" ");
            self.dump_value(arg);
        }
        if !pat.conditions().is_empty() {
            print!(" ?");
            for cond in pat.conditions() {
                print!(" {}{}", ["", "~"][cond.invert() as usize], cond.name());
            }
        }
        println!();
    }

    fn dump_overlap(&self, overlap: &Overlap, depth: usize) {
        use decodetree::OverlapItem as E;
        align(depth);
        println!("{{ # {:04x}:{:04x}", overlap.opcode(), overlap.mask());
        for item in overlap.iter() {
            match item {
                E::Pattern(pattern) => self.dump_pattern(pattern, depth + 1),
                E::Group(group) => self.dump_group(group, depth + 1),
            }
        }
        align(depth);
        println!("}}");
    }

    fn dump_group(&self, group: &Group, depth: usize) {
        use decodetree::Item as E;
        align(depth);
        println!("[ # {:04x}:{:04x}", group.opcode(), group.mask());
        for item in group.iter() {
            match item {
                E::Pattern(pattern) => self.dump_pattern(pattern, depth + 1),
                E::Overlap(overlap) => self.dump_overlap(overlap, depth + 1),
                E::Group(group) => self.dump_group(group, depth + 1),
            }
        }
        align(depth);
        println!("]");
    }

    fn dump_tree(&self) {
        println!("# {}", self.cli.path);
        println!();
        self.dump_fields();
        self.dump_args();
        self.dump_group(self.tree.root(), 0);
    }

    fn dump(&mut self) {
        if self.cli.optimize {
            self.tree.optimize();
        }

        if self.cli.dump {
            if self.cli.debug {
                println!("{:#?}", self.tree);
            } else {
                self.dump_tree();
            }
        }

        if self.cli.generate {
            #[cfg(not(feature = "gen"))]
            {
                eprintln!("error: build with --features gen");
                process::exit(1);
            }

            #[cfg(feature = "gen")]
            {
                if self.cli.dump {
                    println!();
                }

                decodetree::Generator::builder()
                    .variable_size(self.cli.variable_size)
                    .build(&self.tree, ())
                    .generate(&mut std::io::stdout())
                    .unwrap();
            }
        }
    }
}

#[derive(Default)]
struct Cli {
    dump: bool,
    debug: bool,
    optimize: bool,
    generate: bool,
    raw: bool,
    variable_size: bool,
    path: String,
}

fn parse_cli() -> Cli {
    let mut cli = Cli::default();
    for arg in env::args().skip(1) {
        match arg.as_str() {
            "-d" => cli.dump = true,
            "-D" => {
                cli.dump = true;
                cli.debug = true;
            }
            "-O" => cli.optimize = true,
            "-g" => cli.generate = true,
            "-r" => cli.raw = true,
            "-v" => cli.variable_size = true,
            "-h" => {
                println!("parse -d -D -O -g path");
                process::exit(0);
            }
            _ => {
                cli.path = arg;
                return cli;
            }
        }
    }
    cli.path = "test.decode".to_string();
    cli
}

fn main() {
    let mut cli = parse_cli();
    if !cli.dump && !cli.generate {
        cli.dump = true;
    }

    let src = fs::read_to_string(&cli.path).unwrap_or_else(|err| {
        eprintln!("error: failed to read file {}", cli.path);
        eprintln!("{err:?}");
        process::exit(1);
    });

    let parser = Parser::new(&src).set_insn_size(&[8, 16, 24, 32]);

    match parser.parse() {
        Ok(tree) => {
            let mut dump = Dump { cli, tree };
            dump.dump();
        }
        Err(errors) => {
            for e in errors.iter(&cli.path) {
                eprintln!("{}", e);
            }
            process::exit(1);
        }
    }
}
