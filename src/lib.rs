#[macro_use]
extern crate nom;
extern crate lazy_static;
extern crate ordered_float;

#[macro_use]
pub mod util;
pub mod alpha;
pub mod assoc;
pub mod beta;
pub mod closure;
pub mod const_fold;
pub mod elim;
pub mod id;
pub mod inline;
pub mod k_normal;
pub mod parser;
pub mod syntax;
pub mod typing;
pub mod x86_64;
