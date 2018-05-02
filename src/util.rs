macro_rules! build_set {
    ($($x:expr),*) => ({
        let mut h = ::std::collections::HashSet::new();
        $(h.insert($x.clone());)*
            h
    })
}
