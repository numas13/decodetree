mod utils;

#[allow(dead_code)]
pub mod generated {
    use crate::utils::zextract;

    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

#[allow(dead_code)]
pub mod generated_opt {
    use crate::generated::Opcode;
    use crate::utils::zextract;

    include!(concat!(env!("OUT_DIR"), "/generated_opt.rs"));
}
