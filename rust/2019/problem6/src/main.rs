use input::get_input;
use graph::Graph;

fn main() {
    let input = get_input("input");
    let input = parse(input.as_str());
    let solution = solve(&input);
    println!("The solution to part 1 is {}", solution);
}

fn parse(input: &str) -> Vec<Orbit> {
    input.split("\n")
        .map(|s| parse_orbit(s))
        .collect()
}

fn parse_orbit(s: &str) -> Orbit {
    let ns: Vec<&str> = s.split(")")
        .collect();

    Orbit::new(ns[0], ns[1])
}

fn solve(input: &[Orbit]) -> i32 {
    let graph = build_graph(input, Graph::new());

    0
}

fn build_graph(orbits: &[Orbit], graph: Graph<String>) -> Graph<String> {
    if orbits.len() == 0 {
        graph
    } else {
        let orbit = &orbits[0];
        let g = graph.add_edge(orbit.parent.as_str(), orbit.child.as_str());
        build_graph(&orbits[1..], g)
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
        assert_eq!(parse("COM)A\nA)B"), vec![Orbit::new("COM", "A"), Orbit::new("A", "B")]);
    }

    #[test]
    fn test_solve_example() {
        let example_input = get_input("example");
        let example_input = parse(example_input.as_str());
        let solution = solve(&example_input);
        assert_eq!(solution, 42);
    }
}
