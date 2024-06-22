use std::{env, fs};

fn main() {
    let args: Vec<_> = env::args().collect();
    let path = args[1].as_str();
    let src = fs::read_to_string(path).unwrap();
    let res = decodetree::Parser::<u32>::new(&src)
        .set_insn_fixed_size(false)
        .parse();
    match res {
        Ok(tree) => {
            println!("{tree:#?}");
        }
        Err(errors) => {
            for e in errors.iter(path) {
                eprintln!("{}", e);
            }
        }
    }
}
