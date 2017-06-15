#[macro_use]
extern crate nom;
extern crate ordered_float;
extern crate lazy_static;

pub mod syntax;
pub mod parser;
pub mod id;
pub mod typing;
pub mod k_normal;
pub mod alpha;
pub mod beta;
pub mod assoc;
pub mod elim;
