macro_rules! build_set {
    () => (::std::collections::HashSet::new());
    ($($x:expr),+) => ({
        let mut h = ::std::collections::HashSet::new();
        $(h.insert($x.clone());)+
            h
    })
}
