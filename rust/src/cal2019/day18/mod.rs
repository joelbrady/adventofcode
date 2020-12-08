use std::cmp::Ordering;
use std::collections::{BinaryHeap, BTreeSet, HashMap};
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let answer = solve(&input);
    println!("The solution to part 1 is {}", answer);

    let answer2 = solve2(&input);
    println!("The solution to part 2 is {}", answer2);
}

#[derive(Debug, Clone, Eq)]
struct Node {
    distance: u32,
    keys_collected: Rc<BTreeSet<char>>,
    robot_positions: Vec<Position>,
    keys_remaining: u32,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.distance.cmp(&self.distance)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}


impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.robot_positions == other.robot_positions && self.keys_collected == other.keys_collected
    }
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.keys_collected.hash(state);
        self.robot_positions.hash(state);
    }
}

fn solve(input: &InitialState) -> u32 {
    let graph = build_graph(input);

    let start_node = Node {
        distance: 0,
        robot_positions: vec![input.start],
        keys_collected: Rc::new(BTreeSet::new()),
        keys_remaining: input.num_keys,
    };

    dijkstra(&graph, start_node)
}

fn solve2(input: &InitialState) -> u32 {
    let start = input.start;
    let mut map = input.map.clone();
    map.tiles[start.row][start.col] = Tile::StoneWall;
    map.tiles[start.row - 1][start.col] = Tile::StoneWall;
    map.tiles[start.row + 1][start.col] = Tile::StoneWall;
    map.tiles[start.row][start.col - 1] = Tile::StoneWall;
    map.tiles[start.row][start.col + 1] = Tile::StoneWall;

    let robot_positions = vec![
        Position { row: start.row - 1, col: start.col - 1 },
        Position { row: start.row + 1, col: start.col - 1 },
        Position { row: start.row - 1, col: start.col + 1 },
        Position { row: start.row + 1, col: start.col + 1 },
    ];

    robot_positions.iter()
        .for_each(|p| {
            map.tiles[p.row][p.col] = Tile::Start;
        });

    let input = InitialState {
        num_keys: input.num_keys,
        start,
        map,
    };

    let graph = build_graph(&input);

    let start_node = Node {
        distance: 0,
        robot_positions,
        keys_collected: Rc::new(BTreeSet::new()),
        keys_remaining: input.num_keys,
    };

    dijkstra(&graph, start_node)
}

fn dijkstra(graph: &Graph, start_node: Node) -> u32 {
    let mut queue: BinaryHeap<Node> = BinaryHeap::new();
    let mut seen: HashSet<Node> = HashSet::new();

    queue.push(start_node);

    while let Some(node) = queue.pop() {
        if is_terminal(&node) {
            return node.distance;
        }

        if seen.contains(&node) {
            continue;
        }

        seen.insert(node.clone());

        for robot in 0..node.robot_positions.len() {
            let robot_position = &node.robot_positions[robot];
            let edges = graph.get(robot_position).unwrap();
            for (p, tile, distance) in edges.iter() {
                match tile {
                    Tile::Start => {
                        let new_node = move_robot(&node, robot, p, *distance);
                        queue.push(new_node);
                    }
                    Tile::Door(label) => {
                        if node.keys_collected.contains(label) {
                            let new_node = move_robot(&node, robot, p, *distance);
                            queue.push(new_node);
                        }
                    }
                    Tile::Key(label) => {
                        if node.keys_collected.contains(label) {
                            let new_node = move_robot(&node, robot, p, *distance);
                            queue.push(new_node);
                        } else {
                            let mut new_positions = node.robot_positions.clone();
                            new_positions[robot] = *p;
                            let mut keys_collected: BTreeSet<char> = (*node.keys_collected).clone();
                            keys_collected.insert(*label);
                            let new_node = Node {
                                robot_positions: new_positions,
                                distance: node.distance + distance,
                                keys_remaining: node.keys_remaining - 1,
                                keys_collected: Rc::new(keys_collected),
                            };
                            queue.push(new_node);
                        }
                    }
                    _ => unimplemented!("unexpected tile in simplified graph"),
                }
            }
        }
    }

    unimplemented!("no solution possible")
}

fn move_robot(parent: &Node, robot: usize, p: &Position, distance: u32) -> Node {
    let mut new_positions = parent.robot_positions.clone();

    new_positions[robot] = *p;

    Node {
        robot_positions: new_positions,
        distance: parent.distance + distance,
        keys_remaining: parent.keys_remaining,
        keys_collected: Rc::clone(&parent.keys_collected),
    }
}

type Graph = HashMap<Position, Vec<(Position, Tile, u32)>>;

fn build_graph(input: &InitialState) -> Graph {
    let map = &input.map.tiles;
    let mut graph = HashMap::new();

    for (row, row_vec) in map.iter().enumerate() {
        for (col, tile) in row_vec.iter().enumerate() {
            match tile {
                Tile::OpenPassage => continue,
                Tile::StoneWall => continue,
                _ => {
                    let position = Position { row, col };
                    let edges = get_edges(input, &position);
                    graph.insert(position, edges);
                }
            }
        }
    }

    graph
}

fn get_edges(input: &InitialState, position: &Position) -> Vec<(Position, Tile, u32)> {
    let mut queue: VecDeque<(Position, u32)> = VecDeque::new();

    let mut seen: HashSet<Position> = HashSet::new();

    position.around()
        .into_iter()
        .for_each(|p| queue.push_back((p, 1)));

    seen.insert(*position);

    let mut edges = vec![];

    while let Some((position, distance)) = queue.pop_front() {
        if seen.contains(&position) {
            continue;
        }
        let Position { row, col } = position;
        let tile = input.map.tiles[row][col];
        match tile {
            Tile::OpenPassage => {
                position.around()
                    .into_iter()
                    .for_each(|p| queue.push_back((p, distance + 1)));
            }
            Tile::StoneWall => continue,
            _ => {
                edges.push((position, tile, distance));
            }
        }

        seen.insert(position);
    }

    edges
}

fn is_terminal(node: &Node) -> bool {
    node.keys_remaining == 0
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Tile {
    OpenPassage,
    Door(char),
    Key(char),
    StoneWall,
    Start,
}

impl Tile {
    fn parse_char(c: char) -> Tile {
        match c {
            '#' => Tile::StoneWall,
            '.' => Tile::OpenPassage,
            '@' => Tile::Start,
            _ => parse_key_or_door(c)
        }
    }
}

fn parse_key_or_door(c: char) -> Tile {
    if c >= 'a' && c <= 'z' {
        Tile::Key(c)
    } else if c >= 'A' && c <= 'Z' {
        Tile::Door(c.to_ascii_lowercase())
    } else {
        panic!()
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Map {
    tiles: Vec<Vec<Tile>>
}

impl Map {
    // fn display(&self, node: &Node) {
    //     let height = self.tiles.len();
    //     let width = self.tiles[0].len();
    //
    //     for row in 0..height {
    //         for col in 0..width {
    //             let p = Position { row, col };
    //             if node.robot_positions.contains(&p) {
    //                 print!("@")
    //             } else {
    //                 let tile = self.tiles[row][col];
    //                 match tile {
    //                     Tile::OpenPassage => print!("."),
    //                     Tile::Door(label) => {
    //                         if !node.keys_collected.contains(&(label.to_ascii_uppercase())) {
    //                             print!("{}", label.to_ascii_uppercase())
    //                         } else {
    //                             print!(".")
    //                         }
    //                     }
    //                     Tile::Key(label) => {
    //                         if !node.keys_collected.contains(&(label.to_ascii_uppercase())) {
    //                             print!("{}", label.to_ascii_lowercase())
    //                         } else {
    //                             print!(".")
    //                         }
    //                     }
    //                     Tile::Start => print!("@"),
    //                     Tile::StoneWall => print!("#"),
    //                 }
    //             }
    //         }
    //         println!();
    //     }
    //     println!();
    // }
}

#[derive(Copy, Clone, Debug, PartialEq, Hash, Eq)]
struct Position {
    row: usize,
    col: usize,
}

impl Position {
    fn around(&self) -> Vec<Position> {
        let up = Position { row: self.row - 1, col: self.col };
        let down = Position { row: self.row + 1, col: self.col };
        let left = Position { row: self.row, col: self.col - 1 };
        let right = Position { row: self.row, col: self.col + 1 };
        vec![up, down, left, right]
    }
}

#[derive(Debug)]
struct InitialState {
    start: Position,
    map: Map,
    num_keys: u32,
}

fn parse_input(input: &str) -> InitialState {
    let map: Vec<Vec<Tile>> = input.lines()
        .map(|line| parse_row(line))
        .collect();

    let map = Map {
        tiles: map
    };

    let start = find_start_position(&map);

    let num_keys = map.tiles.iter()
        .flat_map(|a| a.iter())
        .filter(|t| matches!(t, Tile::Key(_)))
        .count();

    InitialState {
        start,
        map,
        num_keys: num_keys as u32,
    }
}

fn parse_row(row: &str) -> Vec<Tile> {
    row.chars()
        .map(Tile::parse_char)
        .collect()
}

fn find_start_position(input: &Map) -> Position {
    let map = &input.tiles;

    for (row, row_vec) in map.iter().enumerate() {
        for (col, tile) in row_vec.iter().enumerate() {
            match tile {
                Tile::Start => {
                    return Position { row, col };
                }
                _ => continue,
            }
        }
    }

    panic!("no start tile")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_example() {
        let example_input = include_str!("example1");

        let actual = parse_input(example_input);

        assert_eq!(actual.start, Position { row: 1, col: 5 });
    }

    #[test]
    fn test_solve_example() {
        let example_input = include_str!("example1");
        let example_input = parse_input(example_input);
        let actual = solve(&example_input);
        let expected = 8;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_example2() {
        let example_input = include_str!("example2");
        let example_input = parse_input(example_input);
        let actual = solve(&example_input);
        let expected = 86;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_example3() {
        let example_input = include_str!("example3");
        let example_input = parse_input(example_input);
        let actual = solve(&example_input);
        let expected = 132;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_example4() {
        let example_input = include_str!("example4");
        let example_input = parse_input(example_input);
        let actual = solve(&example_input);
        let expected = 136;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_example5() {
        let example_input = include_str!("example5");
        let example_input = parse_input(example_input);
        let actual = solve(&example_input);
        let expected = 81;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve() {
        let example_input = include_str!("input");
        let example_input = parse_input(example_input);
        let actual = solve(&example_input);
        let expected = 3512;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_part2() {
        let example_input = include_str!("part2_example1");
        let example_input = parse_input(example_input);
        let actual = solve2(&example_input);
        let expected = 72;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve2() {
        let example_input = include_str!("input");
        let example_input = parse_input(example_input);
        let actual = solve2(&example_input);
        let expected = 1514;

        assert_eq!(actual, expected)
    }
}