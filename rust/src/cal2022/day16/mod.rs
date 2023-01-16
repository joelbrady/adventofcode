use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending};
use nom::IResult;
use nom::multi::separated_list1;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(input);

    println!("The solution to part 1 is {}", part1);

    let input = include_str!("input");
    let input = parse_input(input);
    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input<'a> {
    rooms: Vec<Room<'a>>,
}

#[derive(Debug)]
struct Room<'a> {
    label: &'a str,
    flow_rate: i64,
    tunnels: Vec<&'a str>,
}

fn parse_input(s: &str) -> Input {
    let (_, rooms) = separated_list1(line_ending, parse_room)(s).unwrap();

    Input { rooms }
}

fn parse_room(s: &str) -> IResult<&str, Room> {
    let (rem, _) = tag("Valve ")(s)?;
    let (rem, label) = alpha1(rem)?;
    let (rem, _) = tag(" has flow rate=")(rem)?;
    let (rem, flow_rate) = nom::character::complete::i64(rem)?;
    let mut tunnels_prefixes = alt((
        tag("; tunnels lead to valves "),
        tag("; tunnel leads to valve "),
    ));
    let (rem, _) = tunnels_prefixes(rem)?;
    let (rem, tunnels) = separated_list1(tag(", "), alpha1)(rem)?;

    let room = Room {
        tunnels,
        label,
        flow_rate,
    };

    Ok((rem, room))
}

#[derive(Debug, Eq, PartialEq)]
struct Node<'a> {
    room: &'a str,
    steps: i64,
    pressure: i64,
    pressure_rate: i64,
    open_valves: Rc<HashSet<&'a str>>,
}

impl<'a> Hash for Node<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.room.hash(state);
        self.steps.hash(state);
        self.pressure.hash(state);
        self.pressure_rate.hash(state);
        for v in self.open_valves.iter().sorted() {
            v.hash(state);
        }
    }
}

fn solve_part1(input: Input) -> i64 {
    let rooms: HashMap<&str, Room> = input.rooms.into_iter()
        .map(|room| (room.label, room))
        .collect();

    // calculate starting nodes where pressure is > 0, start at steps from AA
    let mut queue = starting_nodes(&rooms);

    // calculate simpler graph where nodes with no flow are removed, new edges could have weights > 1
    let edges: HashMap<&str, Vec<(&str, i64)>> = calculate_simplified_graph(&rooms);

    let mut end_nodes = vec![];
    let mut seen = HashSet::new();

    while let Some(current) = queue.pop_back() {
        if current.steps > 30 {
            continue;
        }
        if seen.contains(&current) {
            continue;
        }
        if current.steps == 30 {
            end_nodes.push(current.pressure);
        } else {
            let new_nodes = generate_children(&rooms, &current, &edges);
            queue.extend(new_nodes);
        }
        seen.insert(current);
    }

    end_nodes.into_iter()
        .max()
        .unwrap()
}

fn starting_nodes<'a>(rooms: &HashMap<&'a str, Room>) -> VecDeque<Node<'a>> {
    rooms.iter()
        .filter(|(_, room)| room.flow_rate > 0)
        .map(|(label, _)| *label)
        .map(|label| (label, bfs(rooms, label)))
        .map(|(room, steps)| Node {
            room,
            pressure: 0,
            pressure_rate: 0,
            open_valves: Rc::new(HashSet::new()),
            steps,
        })
        .collect()
}

fn bfs(rooms: &HashMap<&str, Room>, target: &str) -> i64 {
    struct Node<'a> {
        label: &'a str,
        distance: i64,
    }
    let mut queue = VecDeque::new();
    let start = Node {
        label: "AA",
        distance: 0,
    };
    queue.push_back(start);

    while let Some(current) = queue.pop_front() {
        if current.label == target {
            return current.distance;
        }

        let room = rooms.get(current.label).unwrap();
        for n in &room.tunnels {
            let new = Node {
                label: n,
                distance: current.distance + 1,
            };
            queue.push_back(new);
        }
    }

    panic!("could not find path to {target}")
}

fn calculate_simplified_graph<'a>(rooms: &HashMap<&'a str, Room>) -> HashMap<&'a str, Vec<(&'a str, i64)>> {
    let mut edges: HashMap<&str, Vec<(&str, i64)>> = HashMap::new();

    for start in rooms.keys() {
        for end in rooms.keys() {
            let a = rooms.get(start).unwrap();
            let b = rooms.get(end).unwrap();

            if a.flow_rate == 0 || b.flow_rate == 0 {
                continue;
            }
            if start != end {
                if let Some(d) = path_via_dead_rooms(rooms, start, end, 0, HashSet::new()) {
                    assert_ne!(d, 0);
                    edges.entry(start).or_default().push((end, d));
                }
            }
        }
    }

    edges
}

fn path_via_dead_rooms(rooms: &HashMap<&str, Room>, start: &str, end: &str, d: i64, visited: HashSet<&str>) -> Option<i64> {
    if visited.contains(start) {
        return None;
    }

    if start == end {
        dbg!(start, end, d);
        return Some(d);
    }

    for neighbour in &rooms.get(start).unwrap().tunnels {
        let mut visited = visited.clone();
        visited.insert(start);
        let n = rooms.get(neighbour).unwrap();
        if n.flow_rate == 0 {
            if let Some(d) = path_via_dead_rooms(rooms, neighbour, end, d + 1, visited) {
                return Some(d);
            }
        } else if n.label == end {
            return Some(d + 1);
        }
    }

    None
}

fn generate_children<'a>(rooms: &HashMap<&'a str, Room>, current: &Node<'a>, edges: &HashMap<&'a str, Vec<(&'a str, i64)>>) -> Vec<Node<'a>> {
    let mut new_nodes = vec![];
    // do nothing
    let new = Node {
        room: current.room,
        open_valves: Rc::clone(&current.open_valves),
        steps: current.steps + 1,
        pressure_rate: current.pressure_rate,
        pressure: current.pressure + current.pressure_rate,
    };
    new_nodes.push(new);

    // move to adjacent rooms
    for (adj_room, d) in edges.get(current.room).unwrap() {
        let new = Node {
            room: adj_room,
            steps: current.steps + d,
            pressure_rate: current.pressure_rate,
            pressure: current.pressure + (current.pressure_rate * d),
            open_valves: Rc::clone(&current.open_valves),
        };
        new_nodes.push(new);
    }

    // open valves if > 0
    let room = rooms.get(current.room).unwrap();
    if room.flow_rate > 0 && !current.open_valves.contains(room.label) {
        let mut open_valves = (*current.open_valves).clone();
        open_valves.insert(current.room);
        let new = Node {
            room: current.room,
            pressure: current.pressure_rate + current.pressure,
            steps: current.steps + 1,
            open_valves: Rc::new(open_valves),
            pressure_rate: current.pressure_rate + room.flow_rate,
        };
        new_nodes.push(new);
    }

    new_nodes
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example_children1() {
        let input = parse_input(include_str!("example"));
        let rooms: HashMap<&str, Room> = input.rooms.into_iter()
            .map(|room| (room.label, room))
            .collect();
        let edges: HashMap<&str, Vec<(&str, i64)>> = calculate_simplified_graph(&rooms);
        let start = Node {
            room: "DD",
            open_valves: Rc::new(HashSet::new()),
            steps: 1,
            pressure: 0,
            pressure_rate: 0,
        };

        let mut expected = vec![
            Node {
                room: "DD",
                open_valves: Rc::new(HashSet::new()),
                steps: 2,
                pressure: 0,
                pressure_rate: 0,
            },
            Node {
                room: "BB",
                open_valves: Rc::new(HashSet::new()),
                steps: 3,
                pressure: 0,
                pressure_rate: 0,
            },
            Node {
                room: "CC",
                open_valves: Rc::new(HashSet::new()),
                steps: 2,
                pressure: 0,
                pressure_rate: 0,
            },
            Node {
                room: "JJ",
                open_valves: Rc::new(HashSet::new()),
                steps: 4,
                pressure: 0,
                pressure_rate: 0,
            },
            Node {
                room: "EE",
                open_valves: Rc::new(HashSet::new()),
                steps: 2,
                pressure: 0,
                pressure_rate: 0,
            },
            Node {
                room: "DD",
                open_valves: Rc::new(["DD"].into_iter().collect()),
                steps: 2,
                pressure: 0,
                pressure_rate: 20,
            },
        ];
        expected.sort_by_key(|n| n.room);

        let mut actual = generate_children(&rooms, &start, &edges);
        actual.sort_by_key(|n| n.room);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_example_children2() {
        let input = parse_input(include_str!("example"));
        let rooms: HashMap<&str, Room> = input.rooms.into_iter()
            .map(|room| (room.label, room))
            .collect();
        let edges: HashMap<&str, Vec<(&str, i64)>> = calculate_simplified_graph(&rooms);
        let start = Node {
            room: "DD",
            open_valves: Rc::new(["DD"].into_iter().collect()),
            steps: 2,
            pressure: 0,
            pressure_rate: 20,
        };

        let actual = generate_children(&rooms, &start, &edges);

        assert!(actual.iter().any(|n| {
            *n == Node {
                room: "CC",
                steps: 3,
                pressure: 20,
                pressure_rate: 20,
                open_valves: open_valves(["DD"])
            }
        }))
    }

    #[test]
    fn test_example_children3() {
        let input = parse_input(include_str!("example"));
        let rooms: HashMap<&str, Room> = input.rooms.into_iter()
            .map(|room| (room.label, room))
            .collect();
        let edges: HashMap<&str, Vec<(&str, i64)>> = calculate_simplified_graph(&rooms);
        let start = Node {
            room: "CC",
            steps: 3,
            pressure: 20,
            pressure_rate: 20,
            open_valves: open_valves(["DD"])
        };

        let actual = generate_children(&rooms, &start, &edges);

        assert!(actual.iter().any(|n| {
            *n == Node {
                room: "BB",
                steps: 4,
                pressure: 40,
                pressure_rate: 20,
                open_valves: open_valves(["DD"])
            }
        }))
    }

    #[test]
    fn test_example_children4() {
        let input = parse_input(include_str!("example"));
        let rooms: HashMap<&str, Room> = input.rooms.into_iter()
            .map(|room| (room.label, room))
            .collect();
        let edges: HashMap<&str, Vec<(&str, i64)>> = calculate_simplified_graph(&rooms);
        let start = Node {
            room: "BB",
            steps: 4,
            pressure: 40,
            pressure_rate: 20,
            open_valves: open_valves(["DD"])
        };

        let actual = generate_children(&rooms, &start, &edges);

        assert!(actual.iter().any(|n| {
            *n == Node {
                room: "BB",
                steps: 5,
                pressure: 60,
                pressure_rate: 33,
                open_valves: open_valves(["BB", "DD"])
            }
        }))
    }

    fn open_valves<'a, T>(vs: T) -> Rc<HashSet<&'a str>>
    where T: IntoIterator<Item = &'a str> {
        Rc::new(vs.into_iter().collect())
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 1651;
        let actual = solve_part1(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 1845;
        let actual = solve_part1(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}