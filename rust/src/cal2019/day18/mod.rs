use std::cmp::Ordering;
use std::collections::{BinaryHeap, BTreeSet};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/*
TODO
try building a simpler (weighted) graph, where each node is a door, the edges have weights of the number of steps between nodes
should be much simpler than this giant A*
 */
pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let answer = solve(&input);
    println!("The solution to part 1 is {}", answer);

    let answer2 = solve2(&input);
    println!("The solution to part 2 is {}", answer2);
}

fn solve(input: &InitialState) -> u32 {
    let start = Node {
        map: Rc::new(input.map.clone()),
        keys_collected: BTreeSet::new(),
        robot_positions: vec![input.start],
        steps: 0,
        keys_remaining: input.num_keys as u32,
    };

    solve_any(start, &input.map)
}

fn solve2(input: &InitialState) -> u32 {
    let start = input.start;
    let robot_positions = vec![
        Position { row: start.row - 1, col: start.col - 1 },
        Position { row: start.row + 1, col: start.col - 1 },
        Position { row: start.row - 1, col: start.col + 1 },
        Position { row: start.row + 1, col: start.col + 1 },
    ];

    let mut map = input.map.clone();
    map.tiles[start.row][start.col] = Tile::StoneWall;
    map.tiles[start.row - 1][start.col] = Tile::StoneWall;
    map.tiles[start.row + 1][start.col] = Tile::StoneWall;
    map.tiles[start.row][start.col - 1] = Tile::StoneWall;
    map.tiles[start.row][start.col + 1] = Tile::StoneWall;

    let start_node = Node {
        map: Rc::new(map.clone()),
        keys_collected: BTreeSet::new(),
        keys_remaining: input.num_keys,
        steps: 0,
        robot_positions,
    };

    solve_any(start_node, &map)
}

fn solve_any(start: Node, map: &Map) -> u32 {
    let mut queue: BinaryHeap<Rc<Node>> = BinaryHeap::new();
    let mut seen: HashSet<Rc<Node>> = HashSet::new();

    queue.push(Rc::new(start));

    while let Some(node) = queue.pop() {
        if seen.contains(&node) {
            continue;
        }
        // map.display(&node);
        if is_terminal(&node) {
            return node.steps;
        }
        seen.insert(Rc::clone(&node));
        let children = find_children(&node, map);
        children.into_iter().for_each(|n| queue.push(Rc::new(n)))
    }

    panic!("Could not find solution")
}

fn find_children(node: &Node, map: &Map) -> Vec<Node> {
    let mut new_nodes = vec![];

    for robot in 0..node.robot_positions.len() {
        let current_pos = node.robot_positions[robot];
        current_pos.around()
            .into_iter()
            .map(|pos| (pos, map.get_at_position(&pos)))
            // skip tiles not in bounds
            .filter_map(|(pos, maybe_tile)| maybe_tile.map(|tile| (pos, tile)))
            .filter_map(|(pos, tile)| match tile {
                Tile::Key(label) => {
                    if !node.keys_collected.contains(&(label.to_ascii_uppercase())) {
                        Some(get_key(label, &pos, node, robot))
                    } else {
                        Some(move_to_new_position(&pos, node, robot))
                    }
                }
                Tile::OpenPassage => Some(move_to_new_position(&pos, node, robot)),
                Tile::Door(label) => if node.keys_collected.contains(&(label.to_ascii_uppercase())) {
                    Some(move_to_new_position(&pos, node, robot))
                } else {
                    None
                }
                _ => None,
            })
            .for_each(|node| new_nodes.push(node));
    }

    new_nodes
}

fn get_key(key: char, new_position: &Position, current: &Node, robot: usize) -> Node {
    let mut keys_collected = current.keys_collected.clone();
    keys_collected.insert(key.to_ascii_uppercase());

    let mut new_positions = current.robot_positions.clone();
    new_positions[robot] = *new_position;

    Node {
        map: Rc::clone(&current.map),
        keys_collected,
        steps: current.steps + 1,
        robot_positions: new_positions,
        keys_remaining: current.keys_remaining - 1,
    }
}

fn move_to_new_position(new_position: &Position, current: &Node, robot: usize) -> Node {
    let mut robot_positions = current.robot_positions.clone();
    robot_positions[robot] = *new_position;
    Node {
        map: Rc::clone(&current.map),
        keys_collected: current.keys_collected.clone(),
        robot_positions,
        steps: current.steps + 1,
        keys_remaining: current.keys_remaining,
    }
}

fn is_terminal(node: &Node) -> bool {
    node.keys_remaining == 0
}

#[derive(Debug, Clone, Eq)]
struct Node {
    // could store CoW
    map: Rc<Map>,
    keys_collected: BTreeSet<char>,
    robot_positions: Vec<Position>,
    steps: u32,
    keys_remaining: u32,
}

impl Node {
    fn sum_of_each_robots_max_manhattan_distance(&self) -> u32 {
        self.robot_positions.iter()
            .map(|pos| self.find_max_manhattan_to_any_key_in_zone(pos))
            .sum()
    }

    fn find_max_manhattan_to_any_key_in_zone(&self, robot_position: &Position) -> u32 {
        let height = self.map.tiles.len();
        let width = self.map.tiles[0].len();

        let mut max = 0usize;

        if robot_position.row < height / 2 {
            if robot_position.col < width / 2 {
                // top left
                self.check_quadrant(robot_position, &mut max, 0, height / 2, 0, width / 2);
            } else {
                // top right
                self.check_quadrant(robot_position, &mut max, 0, height / 2, width / 2, width);
            }
        } else if robot_position.col < width / 2 {
            // bottom left
            self.check_quadrant(robot_position, &mut max, height / 2, height, 0, width / 2);
        } else {
            // bottom right
            self.check_quadrant(robot_position, &mut max, height / 2, height, width / 2, width);
        }

        max as u32
    }

    fn check_quadrant(
        &self,
        robot_position: &Position,
        max: &mut usize,
        row_start: usize,
        row_end: usize,
        col_start: usize,
        col_end: usize,
    ) {
        for row in row_start..row_end {
            for col in col_start..col_end {
                let tile = self.map.tiles[row][col];
                if let Tile::Key(label) = tile {
                    if !self.keys_collected.contains(&label) {
                        let width = usize_abs(robot_position.col, col);
                        let height = usize_abs(robot_position.row, row);
                        let manhattan = width + height;
                        if manhattan > *max {
                            *max = manhattan;
                        }
                    }
                }
            }
        }
    }
}


fn usize_abs(a: usize, b: usize) -> usize {
    if a < b {
        b - a
    } else {
        a - b
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        // let us = self.steps + self.keys_remaining;
        // let them = other.steps + other.keys_remaining;
        let us = self.steps + self.sum_of_each_robots_max_manhattan_distance();
        let them = other.steps + other.sum_of_each_robots_max_manhattan_distance();
        them.cmp(&us)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.robot_positions.hash(state);
        self.keys_collected.hash(state);
        self.keys_remaining.hash(state);
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.robot_positions == other.robot_positions && self.keys_collected == other.keys_collected
    }
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
    fn get_at_position(&self, position: &Position) -> Option<Tile> {
        if position.row >= self.tiles.len() || position.col >= self.tiles[0].len() {
            None
        } else {
            Some(self.tiles[position.row][position.col])
        }
    }

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

    let (map, start) = find_start_position(&map);

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

fn find_start_position(input: &Map) -> (Map, Position) {
    let map = &input.tiles;
    let mut output: Vec<Vec<Tile>> = vec![];

    let mut start: Option<Position> = None;

    for (y, row) in map.iter().enumerate() {
        let mut new_row = vec![];
        for (x, col) in row.iter().enumerate() {
            match col {
                Tile::Start => {
                    start = Some(Position { row: y, col: x });
                    new_row.push(Tile::OpenPassage);
                }
                a => new_row.push(*a)
            }
        }
        output.push(new_row);
    }

    (Map { tiles: output }, start.unwrap())
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
}