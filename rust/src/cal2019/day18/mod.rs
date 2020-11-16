use nom::lib::std::collections::VecDeque;
use std::rc::Rc;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let answer = solve(&input);
}

fn solve(input: &InitialState) -> u64 {
    let start = Node {
        map: input.map.clone(),
        position: input.start,
    };
    let mut queue: VecDeque<Rc<Node>> = VecDeque::new();

    queue.push_back(Rc::new(start));

    while let Some(node) = queue.pop_front() {
        let children = find_children(&node);
        children.into_iter().for_each(|n| queue.push_back(Rc::new(n)))
    }


    unimplemented!()
}

fn find_children(node: &Node) -> Vec<Node> {
    unimplemented!()
}

struct Node {
    map: Map,
    position: Position,
    keys: Vec<char>,
}

#[derive(Copy, Clone, Debug)]
enum Tile {
    OpenPassage,
    Door(char),
    Key(char),
    StoneWall,
    Start
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

#[derive(Debug, Clone)]
struct Map {
    tiles: Vec<Vec<Tile>>
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct Position {
    row: usize,
    col: usize
}

#[derive(Debug)]
struct InitialState {
    start: Position,
    map: Map
}

fn parse_input(input: &str) -> InitialState {
    let map: Vec<Vec<Tile>> = input.lines()
        .map(|line| parse_row(line))
        .collect();

    let map = Map {
        tiles: map
    };

    let (map, start) = find_start_position(&map);

    InitialState {
        start,
        map
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

        assert_eq!(actual.start, Position { row: 1, col: 5});
    }

    #[test]
    fn test_solve_example() {
        let example_input = include_str!("example1");
        let example_input = parse_input(example_input);
        let actual = solve(&example_input);
        let expected = 8u64;

        assert_eq!(actual, expected)
    }
}