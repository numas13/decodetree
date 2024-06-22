use std::{env, fs, process};

use decodetree::{DecodeTree, Group, Overlap, Parser, Pattern};

fn dump_field_item(item: &decodetree::FieldItem) {
    use decodetree::FieldItem as E;
    let (sxt, len) = match item {
        E::Field { pos, len, sxt } => {
            print!("{pos}");
            (sxt, len)
        }
        E::FieldRef { field, len, sxt } => {
            print!("{}", field.name.as_ref().unwrap());
            (sxt, len)
        }
    };
    print!(":{}{len}", ["", "s"][*sxt as usize]);
}

fn dump_fields(tree: &DecodeTree) {
    if tree.fields.is_empty() {
        return;
    }

    let fields = {
        let mut v: Vec<_> = tree.fields.iter().collect();
        v.sort_by_key(|(k, _)| k.as_str());
        v
    };

    println!("# Fields:");
    for (name, field) in fields {
        print!("%{name}");
        for item in &field.items {
            print!(" ");
            dump_field_item(item);
        }
        if let Some(func) = &field.func {
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

    let args = {
        let mut v: Vec<_> = tree.args.values().collect();
        v.sort_by_key(|i| i.name.as_str());
        v
    };

    println!("# Args:");
    for set in args {
        print!("&{}", set.name);
        for arg in &set.items {
            print!(" {}", arg.name);
        }
        if set.is_extern {
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

fn dump_value_kind(name: &str, kind: &decodetree::ValueKind) {
    use decodetree::ValueKind as E;
    match kind {
        E::Set(set) => {
            print!("&{name}");
            for arg in &set.items {
                print!(" ");
                dump_value_kind(&arg.name, arg.value.as_ref().unwrap());
            }
        }
        E::Field(field) => {
            print!("{name}=");
            match &field.name {
                Some(name) => print!("%{name}"),
                None => dump_field_item(&field.items[0]),
            }
        }
        E::Const(value) => {
            println!("{name}={value}");
        }
    }
}

fn dump_pattern(_tree: &DecodeTree, pat: &Pattern, depth: usize) {
    align(depth);
    print!("{:04x}:{:04x} ", pat.opcode, pat.mask);
    print!("{}", pat.name);
    for arg in &pat.args {
        print!(" ");
        dump_value_kind(&arg.name, &arg.kind);
    }
    if !pat.cond.is_empty() {
        print!(" ?");
        for i in &pat.cond {
            print!(" {}{}", ["", "~"][i.invert as usize], i.name);
        }
    }
    println!();
}

fn dump_overlap(tree: &DecodeTree, overlap: &Overlap, depth: usize) {
    use decodetree::OverlapItem as E;
    align(depth);
    println!("{{ # {:04x}:{:04x}", overlap.opcode, overlap.mask);
    for item in &overlap.items {
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
    println!("[ # {:04x}", group.mask);
    for item in &group.items {
        match item {
            E::Pattern(pattern) => dump_pattern(tree, pattern, depth + 1),
            E::Overlap(overlap) => dump_overlap(tree, overlap, depth + 1),
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
    debug: bool,
    path: String,
}

fn parse_cli() -> Cli {
    let mut cli = Cli::default();
    for arg in env::args().skip(1) {
        match arg.as_str() {
            "-d" | "--debug" => cli.debug = true,
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
    let cli = parse_cli();
    let src = fs::read_to_string(&cli.path).unwrap_or_else(|err| {
        eprintln!("error: failed to read file {}", cli.path);
        eprintln!("{err:?}");
        process::exit(1);
    });

    let parser = Parser::new(&src).set_insn_fixed_size(false);

    match parser.parse() {
        Ok(tree) if cli.debug => println!("{tree:#?}"),
        Ok(tree) => dump_tree(&cli, &tree),
        Err(errors) => {
            for e in errors.iter(&cli.path) {
                eprintln!("{}", e);
            }
        }
    }
}
