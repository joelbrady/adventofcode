use std::collections::HashSet;

use crate::cal2019::day06::graph::Graph;

pub fn main() {
    let input = include_str!("input");
    let input = parse(input);
    let solution = solve(&input);
    println!("The solution to part 1 is {}", solution);

    let solution = solve2(&input);
    println!("The solution to part 2 is {}", solution);
}

fn parse(input: &str) -> Vec<Orbit> {
    input.lines()
        .map(parse_orbit)
        .collect()
}

fn parse_orbit(s: &str) -> Orbit {
    let ns: Vec<&str> = s.split(')')
        .collect();

    Orbit::new(ns[0], ns[1])
}

fn solve(input: &[Orbit]) -> i32 {
    let graph = build_graph(input, Graph::new());

    cost("COM", &graph, 0)
}

fn solve2(input: &[Orbit]) -> i32 {
    let graph = build_graph2(input, Graph::new());

    let you = input.iter()
        .find(|o| o.child.as_str() == "YOU")
        .unwrap();

    let san = input.iter()
        .find(|o| o.child.as_str() == "SAN")
        .unwrap();

    bfs(&graph, you, san)
}

fn bfs(graph: &Graph<String>, you: &Orbit, san: &Orbit) -> i32 {
    let start = you.parent.as_str();
    let end = san.parent.as_str();

    let mut queue: Vec<(String, i32)> = vec![(String::from(start), 0)];
    let mut visited: HashSet<String> = HashSet::new();

    while !queue.is_empty() {
        let (node, depth) = queue.pop().unwrap();
        if visited.contains(&node) {
            continue;
        } else {
            visited.insert(node.clone());
        }
        if node == end {
            return depth
        }
        graph.get_children(&node)
            .iter()
            .for_each(|child| {
                queue.push((child.clone(), depth + 1))
            });
    }

    panic!("ran out of nodes")
}

fn cost(n: &str, graph: &Graph<String>, depth: i32) -> i32 {
    let children = graph.get_children(n);
    let costs: i32 = children
        .iter()
        .map(|c| c.as_str())
        .map(|c| cost(c, graph, depth + 1))
        .sum();

    depth + costs
}

fn build_graph(orbits: &[Orbit], graph: Graph<String>) -> Graph<String> {
    if orbits.is_empty() {
        graph
    } else {
        let orbit = &orbits[0];
        let g = graph.add_edge(orbit.parent.as_str(), orbit.child.as_str());
        build_graph(&orbits[1..], g)
    }
}

fn build_graph2(orbits: &[Orbit], graph: Graph<String>) -> Graph<String> {
    if orbits.is_empty() {
        graph
    } else {
        let orbit = &orbits[0];
        let graph = graph.add_edge(orbit.parent.as_str(), orbit.child.as_str());
        let graph = graph.add_edge(orbit.child.as_str(), orbit.parent.as_str());
        build_graph2(&orbits[1..], graph)
    }
}

#[derive(Eq, PartialEq, Debug)]
struct Orbit {
    parent: String,
    child: String,
}

impl Orbit {
    fn new(parent: &str, child: &str) -> Self {
        let parent = String::from(parent);
        let child = String::from(child);
        Orbit { parent, child }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_orbit() {
        assert_eq!(parse_orbit("Z)Y"), Orbit::new("Z", "Y"));
    }

    #[test]
    fn test_parse() {
        assert_eq!(parse("COM)A\r\nA)B"), vec![Orbit::new("COM", "A"), Orbit::new("A", "B")]);
    }

    #[test]
    fn test_solve_example() {
        let example_input = include_str!("example");
        let example_input = parse(example_input);
        let solution = solve(&example_input);
        assert_eq!(solution, 42);
    }

    #[test]
    fn test_solve_part2_example() {
        let input = include_str!("example2");
        let input = parse(input);
        let solution = solve2(&input);
        assert_eq!(solution, 4);
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse(input);
        let solution = solve(&input);
        assert_eq!(solution, 301100);

        let solution = solve2(&input);
        assert_eq!(solution, 547);
    }
}

mod graph {
    use std::collections::HashMap;
    use std::hash::Hash;

    #[derive(Debug)]
    pub struct Graph<N: Eq + Hash + Clone> {
        edges: HashMap<N, Vec<N>>,
    }

    impl Graph<String> {
        pub fn new() -> Graph<String> {
            Graph { edges: HashMap::new() }
        }

        pub fn add_edge<V>(&self, a: V, b: V) -> Self
            where V: Into<String> {
            let a = a.into();
            let b = b.into();
            let mut edges = self.edges.clone();
            let mut children = edges.get(&a)
                .cloned()
                .unwrap_or_default();
            children.push(b);
            edges.insert(a, children);
            Graph { edges }
        }

        #[allow(dead_code)]
        pub fn has_edge<V>(&self, a: V, b: V) -> bool
            where V: Into<String> {
            self.edges.get(&a.into())
                .map(|v| v.contains(&b.into()))
                .unwrap_or(false)
        }

        pub fn get_children<V>(&self, a: V) -> Vec<String>
            where V: Into<String> {
            self.edges.get(&a.into())
                .cloned()
                .unwrap_or_default()
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_add_edge() {
            let g: Graph<String> = Graph::new();
            let g = g.add_edge("a", "b");
            assert!(g.has_edge("a", "b"));
            assert!(!g.has_edge("a", "c"));
            assert!(!g.has_edge("c", "c"));
            assert!(!g.has_edge("b", "a"));

            assert_eq!(g.get_children("a"), vec![String::from("b")]);
            let expected: Vec<String> = vec![];
            assert_eq!(g.get_children("b"), expected);
        }
    }

}