use std::{env, fs};

fn main() {
    let args: Vec<_> = env::args().collect();
    let path = args[1].as_str();
    let src = fs::read_to_string(path).unwrap();
    match decodetree::parse::<u32>(&src) {
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
