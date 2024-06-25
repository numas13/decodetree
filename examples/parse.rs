use std::{env, fs, process};

use decodetree::{DecodeTree, Group, Overlap, Parser, Pattern};

fn dump_field_len(len: u32, sxt: bool) {
    print!("{}{len}", ["", "s"][sxt as usize]);
}

fn dump_unnamed_field(field: &decodetree::UnnamedField) {
    print!("{}:", field.pos());
    dump_field_len(field.len(), field.sxt());
}

fn dump_named_field(field: &decodetree::FieldRef) {
    print!("{}:", field.field().name());
    dump_field_len(field.len(), field.sxt());
}

fn dump_field_item(item: &decodetree::FieldItem) {
    use decodetree::FieldItem as E;
    match item {
        E::Field(f) => dump_unnamed_field(f),
        E::FieldRef(f) => dump_named_field(f),
    }
}

fn dump_field(field: &decodetree::Field) {
    use decodetree::Field as E;
    match field {
        E::Field(f) => dump_unnamed_field(f),
        E::FieldRef(f) => print!("%{}", f.name()),
    }
}

fn dump_fields(tree: &DecodeTree) {
    if tree.fields.is_empty() {
        return;
    }
    println!("# Fields:");
    for field in &tree.fields {
        print!("%{}", field.name());
        for item in field.items() {
            print!(" ");
            dump_field_item(item);
        }
        if let Some(func) = field.func() {
            print!(" !function={func}");
        }
        println!();
    }
    println!();
}

fn dump_args(tree: &DecodeTree) {
    if tree.args.is_empty() {
        return;
    }
    println!("# Args:");
    for set in &tree.args {
        print!("&{}", set.name());
        for arg in set.items() {
            print!(" {}", arg.name());
        }
        if set.is_extern() {
            print!(" !extern");
        }
        println!();
    }
    println!();
}

fn align(depth: usize) {
    for _ in 0..depth {
        print!("  ");
    }
}

fn dump_set_value(value: &decodetree::ArgsValue) {
    use decodetree::ArgsValueKind as E;
    print!("{}=", value.name());
    match value.kind() {
        E::Field(field) => dump_field(field),
        E::Const(value) => print!("{value}"),
    }
}

fn dump_value(value: &decodetree::Value) {
    use decodetree::ValueKind as E;
    let name = value.name();
    match value.kind() {
        E::Args(set) => {
            print!("&{name}");
            for arg in set {
                print!(" ");
                dump_set_value(arg);
            }
        }
        E::Field(field) => {
            print!("{name}=");
            dump_field(field);
        }
        E::Const(value) => {
            print!("{name}={value}");
        }
    }
}

fn dump_pattern(_tree: &DecodeTree, pat: &Pattern, depth: usize) {
    for line in pat.raw().lines() {
        align(depth);
        println!("# {line}");
    }
    align(depth);
    print!("{:04x}:{:04x}:{:<2} ", pat.opcode(), pat.mask(), pat.size());
    print!("{}", pat.name());
    for arg in pat.args() {
        print!(" ");
        dump_value(arg);
    }
    if !pat.conditions().is_empty() {
        print!(" ?");
        for cond in pat.conditions() {
            print!(" {}{}", ["", "~"][cond.invert() as usize], cond.name());
        }
    }
    println!();
}

fn dump_overlap(tree: &DecodeTree, overlap: &Overlap, depth: usize) {
    use decodetree::OverlapItem as E;
    align(depth);
    println!("{{ # {:04x}:{:04x}", overlap.opcode(), overlap.mask());
    for item in overlap.iter() {
        match item {
            E::Pattern(pattern) => dump_pattern(tree, pattern, depth + 1),
            E::Group(group) => dump_group(tree, group, depth + 1),
        }
    }
    align(depth);
    println!("}}");
}

fn dump_group(tree: &DecodeTree, group: &Group, depth: usize) {
    use decodetree::Item as E;
    align(depth);
    println!("[ # {:04x}:{:04x}", group.opcode(), group.mask());
    for item in group.iter() {
        match item {
            E::Pattern(pattern) => dump_pattern(tree, pattern, depth + 1),
            E::Overlap(overlap) => dump_overlap(tree, overlap, depth + 1),
            E::Group(group) => dump_group(tree, group, depth + 1),
        }
    }
    align(depth);
    println!("]");
}

fn dump_tree(cli: &Cli, tree: &DecodeTree) {
    println!("# {}", cli.path);
    println!();
    dump_fields(tree);
    dump_args(tree);
    dump_group(tree, &tree.root, 0);
}

#[derive(Default)]
struct Cli {
    dump: bool,
    debug: bool,
    optimize: bool,
    generate: bool,
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

    let parser = Parser::new(&src).set_insn_fixed_size(false);

    match parser.parse() {
        Ok(mut tree) => {
            if cli.optimize {
                tree.optimize();
            }

            if cli.dump {
                if cli.debug {
                    println!("{tree:#?}");
                } else {
                    dump_tree(&cli, &tree);
                }
            }

            if cli.generate {
                #[cfg(not(feature = "gen"))]
                {
                    eprintln!("error: build with --features gen");
                    process::exit(1);
                }

                #[cfg(feature = "gen")]
                {
                    if cli.dump {
                        println!();
                    }

                    decodetree::Generator::builder()
                        .build(&tree, ())
                        .generate(&mut std::io::stdout())
                        .unwrap();
                }
            }
        }
        Err(errors) => {
            for e in errors.iter(&cli.path) {
                eprintln!("{}", e);
            }
        }
    }
}
