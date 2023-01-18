use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::hash::Hash;

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
    let part2 = solve_part2(input);
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

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct Node {
    room: usize,
    steps: i64,
    pressure: i64,
    open_valves: u32,
}

fn solve_part1(input: Input) -> i64 {
    solve(&input, 30)
        .into_iter()
        .max_by_key(|(p, _)| *p)
        .unwrap()
        .0
}

fn solve(input: &Input, max_steps: i64) -> Vec<(i64, u32)> {
    let Setup {
        rooms,
        distances,
        labels,
    } = setup(input);

    let start_node = Node {
        room: *labels.get("AA").unwrap(),
        steps: max_steps,
        pressure: 0,
        open_valves: 0,
    };

    let mut queue: VecDeque<Node> = [start_node].into_iter().collect();

    let mut end_nodes = vec![];
    let mut seen = HashSet::new();

    while !queue.is_empty() {
        while let Some(current) = queue.pop_back() {
            if current.steps < 0 {
                continue;
            }
            if seen.contains(&current) {
                continue;
            }
            if current.steps == 0 {
                end_nodes.push((current.pressure, current.open_valves));
            } else {
                let new_nodes = generate_children(&rooms, &distances, &current);
                queue.extend(new_nodes);
            }
            seen.insert(current);
        }
    }

    end_nodes
}

struct Graph<'a> {
    labels: HashMap<&'a str, usize>,
    distances: Vec<Vec<i64>>,
}

fn floyd_warshall<'a>(rooms: &HashMap<&'a str, &Room<'a>>) -> Graph<'a> {
    let labels: HashMap<&str, usize> = rooms.iter()
        .sorted_by_key(|(label, room)| {
            if **label == "AA" {
                i64::MIN
            } else {
                -room.flow_rate
            }
        })
        .enumerate()
        .map(|(i, (label, _room))| (*label, i))
        .collect();

    let n = rooms.len();
    let mut distances = vec![vec![i64::MAX / 2 - 1; n]; n];
    for (label, room) in rooms {
        for edge in &room.tunnels {
            let i = labels.get(label).unwrap();
            let j = labels.get(edge).unwrap();
            distances[*i][*j] = 1;
        }
    }

    distances.iter_mut()
        .enumerate()
        .for_each(|(i, v)| v[i] = 0);

    for k in 0..n {
        for i in 0..n {
            for j in 0..n {
                if distances[i][j] > distances[i][k] + distances[k][j] {
                    distances[i][j] = distances[i][k] + distances[k][j];
                }
            }
        }
    }

    Graph { labels, distances }
}

fn generate_children(
    rooms: &HashMap<usize, &Room>,
    edges: &[Vec<i64>],
    current: &Node,
) -> Vec<Node> {
    let mut new_nodes = vec![];

    // do nothing
    let new = Node {
        room: current.room,
        steps: current.steps - 1,
        pressure: current.pressure,
        open_valves: current.open_valves,
    };
    new_nodes.push(new);

    // move to adjacent rooms
    for (adj_room, d) in edges[current.room].iter().enumerate() {
        let room = rooms.get(&adj_room).unwrap();
        if adj_room != current.room && room.flow_rate > 0 {
            let new = Node {
                room: adj_room,
                steps: current.steps - d,
                pressure: current.pressure,
                open_valves: current.open_valves,
            };
            new_nodes.push(new);
        }
    }

    // open valves if > 0
    let room = rooms.get(&current.room).unwrap();
    if current.room > 20 {
        dbg!(current);
        panic!();
    }
    let room_bit_mask = 1 << current.room;
    if room.flow_rate > 0 && current.open_valves & (room_bit_mask) == 0 {
        let time_remaining = current.steps - 1;
        let new = Node {
            room: current.room,
            steps: time_remaining,
            pressure: current.pressure + (room.flow_rate * time_remaining),
            open_valves: current.open_valves | room_bit_mask,
        };
        new_nodes.push(new);
    }

    new_nodes
}

fn solve_part2(input: Input) -> i64 {
    // generate all possible paths in 26 steps
    let paths = solve(&input, 26);

    // remove duplicates
    let paths: BTreeSet<_> = paths.into_iter().collect();

    // keep highest score for each unique set of valves opened
    let mut m: BTreeMap<u32, i64> = BTreeMap::new();
    for (a, b) in paths {
        let c = m.entry(b).or_default();
        let d = *c;
        *c = d.max(a);
    }
    let mut paths: Vec<_> = m.into_iter()
        .map(|(a, b)| (b, a))
        .collect();

    // order by highest score
    paths.sort_by_key(|(score, _)| -*score);

    // iterate over all pairs
    let mut best = 0;
    for (i, (sa, va)) in paths.iter().enumerate() {
        // since this is sorted sb <= sa, so once our best is pretty good we can stop
        if sa * 2 < best {
            break;
        }
        for (sb, vb) in &paths[i + 1..] {
            // check for paths having no valves in common
            if va & vb == 0 {
                best = best.max(sa + sb);
            }
        }
    }

    best
}

struct Setup<'a, 'b> {
    rooms: HashMap<usize, &'b Room<'a>>,
    distances: Vec<Vec<i64>>,
    labels: HashMap<&'a str, usize>,
}

fn setup<'a, 'b>(
    input: &'b Input<'a>,
) -> Setup<'a, 'b> {
    let rooms: HashMap<&str, &Room> = input.rooms.iter()
        .map(|room| (room.label, room))
        .collect();

    // calculate simpler graph where nodes with no flow are removed, new edges could have weights > 1
    let Graph { labels, distances } = floyd_warshall(&rooms);

    let rooms = rooms.into_iter()
        .map(|(k, v)| (*labels.get(k).unwrap(), v))
        .collect();

    Setup { rooms, distances, labels }
}

#[cfg(test)]
mod test {
    use super::*;

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
        let expected = 1707;
        let actual = solve_part2(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        // 2286
        // human path 1377 [VP, VM, TR, DO, KI, HN]
        // elephant path 909 [SH, XQ, GA, MN, QH, VW]
        let expected = 2286;
        let actual = solve_part2(input);

        assert_eq!(actual, expected)
    }
}