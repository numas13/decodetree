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

#[allow(dead_code)]
pub mod generated_vs {
    include!(concat!(env!("OUT_DIR"), "/generated_vs.rs"));
}

#[allow(dead_code)]
pub mod generated_vs_opt {
    use crate::generated_vs::Opcode;

    include!(concat!(env!("OUT_DIR"), "/generated_vs_opt.rs"));
}
