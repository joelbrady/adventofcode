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

    pub fn add_edge(&self, a: &str, b: &str) -> Self {
        let a = String::from(a);
        let b = String::from(b);
        let mut edges = self.edges.clone();
        let mut children = edges.get(&a)
            .cloned()
            .unwrap_or_else(Vec::new);
        children.push(b);
        edges.insert(a, children);
        Graph { edges }
    }

    #[allow(dead_code)]
    pub fn has_edge(&self, a: &str, b: &str) -> bool {
        self.edges.get(a)
            .map(|v| v.contains(&String::from(b)))
            .unwrap_or(false)
    }

    pub fn get_children(&self, a: &str) -> Vec<String> {
        self.edges.get(a)
            .cloned()
            .unwrap_or_else(Vec::new)
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
