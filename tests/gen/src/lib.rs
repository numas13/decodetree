mod utils;

#[allow(dead_code)]
pub mod decode32 {
    use crate::utils::zextract;

    include!(concat!(env!("OUT_DIR"), "/decode32.rs"));
}
