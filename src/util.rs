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

#[derive(PartialEq, Eq, Debug)]
enum MoveType {
    /// a1 -> a2
    Single(usize, usize),
    /// a1 -> a2 -> ... an -> a1
    Loop(Vec<usize>),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum VisitType {
    NotVisited,
    Visiting,
    Visited,
}

fn solve_functional_graph_dfs(
    a: &[usize],
    mvs: &mut Vec<MoveType>,
    visited: &mut [VisitType],
    v: usize,
) -> Option<Vec<usize>> {
    if visited[v] == VisitType::Visiting {
        return Some(vec![v]);
    }
    if visited[v] == VisitType::Visited {
        return None;
    }
    visited[v] = VisitType::Visiting;
    let sub = solve_functional_graph_dfs(a, mvs, visited, a[v]);
    visited[v] = VisitType::Visited;
    if let Some(mut sub) = sub {
        if sub[0] == v {
            sub.reverse();
            mvs.push(MoveType::Loop(sub));
            return None;
        } else {
            sub.push(v);
            return Some(sub);
        }
    }
    mvs.push(MoveType::Single(a[v], v));
    None
}

/// Returns an operation sequence that achieves the desired state `a`.
fn solve_functional_graph(a: &[usize]) -> Vec<MoveType> {
    let n = a.len();
    let mut mvs = vec![];
    let mut visited = vec![VisitType::NotVisited; n];
    for v in 0..n {
        if visited[v] == VisitType::NotVisited {
            let result = solve_functional_graph_dfs(a, &mut mvs, &mut visited, v);
            assert_eq!(result, None);
        }
    }
    // Reversing order is necessary.
    mvs.reverse();
    mvs
}

pub fn assign(a: &[(String, String)], tmp: &str) -> Vec<(String, String)> {
    // Creates a one-to-one mapping between strings and indices
    let mut seen = HashMap::new();
    let mut count = 0;
    let mut name_table = vec![];
    for (x, y) in a {
        if !seen.contains_key(x) {
            seen.insert(x.clone(), count);
            name_table.push(x.clone());
            count += 1;
        }
        if !seen.contains_key(y) {
            seen.insert(y.clone(), count);
            name_table.push(y.clone());
            count += 1;
        }
    }
    // Converts a into mapping between indices
    let mut indmap: Vec<usize> = (0..count).collect();
    for (src, dst) in a {
        let src_index = seen[src];
        let dst_index = seen[dst];
        indmap[dst_index] = src_index;
    }
    let raw_operations = solve_functional_graph(&indmap);
    let mut operations = vec![];
    for op in raw_operations {
        match op {
            MoveType::Loop(regs) => {
                // if length is 1, then no moving is required.
                if regs.len() >= 2 {
                    operations.push((name_table[regs[0]].clone(), tmp.to_string()));
                    for i in (1..regs.len()).rev() {
                        operations.push((
                            name_table[regs[i]].clone(),
                            name_table[regs[(i + 1) % regs.len()]].clone(),
                        ));
                    }
                    operations.push((tmp.to_string(), name_table[regs[1]].clone()));
                }
            }
            MoveType::Single(src, dst) => {
                operations.push((name_table[src].clone(), name_table[dst].clone()));
            }
        }
    }
    assert_assignment(a, &operations);
    operations
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_functional_graph_works_correctly() {
        // Simple loops
        assert_eq!(
            solve_functional_graph(&[0, 1, 2]),
            vec![
                MoveType::Loop(vec![2]),
                MoveType::Loop(vec![1]),
                MoveType::Loop(vec![0]),
            ]
        );

        // 0 -> 1 -> 2
        assert_eq!(
            solve_functional_graph(&[0, 0, 1]),
            vec![
                MoveType::Single(1, 2),
                MoveType::Single(0, 1),
                MoveType::Loop(vec![0]),
            ]
        );

        // 0 <- 1 <- 2
        assert_eq!(
            solve_functional_graph(&[1, 2, 2]),
            vec![
                MoveType::Single(1, 0),
                MoveType::Single(2, 1),
                MoveType::Loop(vec![2]),
            ]
        );

        // 0 -> 1 -> 2 -> 0
        assert_eq!(
            solve_functional_graph(&[1, 2, 0]),
            // The order is not relevant.
            vec![MoveType::Loop(vec![1, 2, 0])],
        );

        // 0 <-> 1 -> 2
        assert_eq!(
            solve_functional_graph(&[1, 0, 1]),
            vec![MoveType::Single(1, 2), MoveType::Loop(vec![1, 0])]
        )
    }
}
