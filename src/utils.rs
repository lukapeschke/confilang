use std::cmp;
use std::hash;
use std::ops;

#[derive(Debug, Clone, PartialEq)]
pub struct HashableFloat(f32);

impl HashableFloat {
    pub fn new(f: f32) -> HashableFloat {
        HashableFloat(f)
    }

    pub fn value(&self) -> f32 {
        self.0
    }
}

// NOTE: Implementing Hash but deriving PartialEq is generally a bad idea since
// the implementations must agree, but the hash implementation panics
// anyway so ¯\_(ツ)_/¯
// https://rust-lang.github.io/rust-clippy/master/index.html#derive_hash_xor_eq
#[allow(clippy::derive_hash_xor_eq)]
impl hash::Hash for HashableFloat {
    fn hash<H: hash::Hasher>(&self, _state: &mut H) {
        panic!("hash not implemented for HashableFloat");
    }
}

impl cmp::Eq for HashableFloat {}

impl ops::Neg for HashableFloat {
    type Output = HashableFloat;

    fn neg(self) -> Self::Output {
        HashableFloat::new(-self.value())
    }
}
