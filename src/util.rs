use std::collections::HashMap;
macro_rules! build_set {
    () => (::std::collections::HashSet::new());
    ($($x:expr),+) => ({
        let mut h = ::std::collections::HashSet::new();
        $(h.insert($x.clone());)+
            h
    })
}

// TODO: error handling
fn assert_assignment(desired: &[(String, String)], operations: &[(String, String)]) {
    let mut counter = 0;
    let mut old = HashMap::new();
    let mut new = HashMap::new();
    for &(ref src, ref dst) in desired {
        old.insert(src.clone(), counter);
        new.insert(dst.clone(), counter);
        counter += 1;
    }
    for &(ref src, ref dst) in operations {
        let val = old[src];
        old.insert(dst.clone(), val);
    }
    for (key, value) in new {
        assert_eq!(old[&key], value);
    }
}

pub fn assign(a: &[(String, String)], tmp: &str) -> Vec<(String, String)> {
    let mut operations = a.to_vec();
    // TODO: Ad-hoc assignment for mere swapping
    if a.len() == 2 && a[0].0 == a[1].1 && a[0].1 == a[1].0 {
        let (x, y) = a[0].clone();
        operations = vec![
            (x.clone(), tmp.to_string()),
            (y.clone(), x.clone()),
            (tmp.to_string(), y.clone()),
        ];
    }
    assert_assignment(a, &operations);
    operations
}
