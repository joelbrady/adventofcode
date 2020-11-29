use std::rc::Rc;

use nom::lib::std::collections::VecDeque;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let answer = solve(&input);

    println!("The solution to part 1 is {}", answer);
}

fn solve(input: &InitialState) -> u64 {
    let start = Node {
        map: input.map.clone(),
        position: input.start,
        steps: 0,
    };
    let mut queue: VecDeque<Rc<Node>> = VecDeque::new();

    queue.push_back(Rc::new(start));

    while let Some(node) = queue.pop_front() {
        node.map.display(&node.position);
        if is_terminal(&node) {
            return node.steps
        }
        let children = find_children(&node);
        children.into_iter().for_each(|n| queue.push_back(Rc::new(n)))
    }

    panic!("Could not find solution")
}

fn find_children(node: &Node) -> Vec<Node> {
    let current_pos = &node.position;
    let map = &node.map;
    let around: Vec<(Position, Tile)> = current_pos.around().into_iter()
        .map(|pos| (pos, map.get_at_position(&pos)))
        .filter_map(|(pos, maybe_tile)| maybe_tile.map(|tile| (pos, tile)))
        .collect();

    // neighbour empty squares
    let empty = around.iter()
        .filter(|(_, tile)| *tile == Tile::OpenPassage)
        .map(|(pos, _)| move_to_new_position(pos, node));

    // neighbour key (remove key + door)
    let gain_key = around.iter()
        .filter_map(|(pos, tile)| match tile {
            Tile::Key(label) => Some(get_key(label, pos, node)),
            _ => None
        });

    empty.chain(gain_key).collect()
}

fn get_key(label: &char, new_position: &Position, current: &Node) -> Node {
    // remove the door with label
    // remove the key with label (it's at new_position)
    // then move to new position
    let mut map = current.map.clone();
    let steps = current.steps + 1;
    let position = *new_position;

    remove_door(&mut map, *label);
    remove_key(&mut map, &new_position);

    Node {
        map,
        steps,
        position,
    }
}

fn remove_door(map: &mut Map, label: char) {
    let height = map.tiles.len();
    let width = map.tiles[0].len();

    for row in 0..height {
        for col in 0..width {
            if map.tiles[row][col] == Tile::Door(label) {
                map.tiles[row][col] = Tile::OpenPassage;
                return
            }
        }
    }
}

fn remove_key(map: &mut Map, position: &Position) {
    map.tiles[position.row][position.col] = Tile::OpenPassage
}

fn move_to_new_position(new_position: &Position, current: &Node) -> Node {
    Node {
        position: *new_position,
        map: current.map.clone(),
        steps: current.steps + 1,
    }
}

fn is_terminal(node: &Node) -> bool {
    let has_key = node.map.tiles.iter()
        .any(|row| row.iter()
            .any(|col| matches!(col, Tile::Key(_))));

    !has_key
}

#[derive(Debug, Clone)]
struct Node {
    map: Map,
    position: Position,
    steps: u64,
}

#[derive(Copy, Clone, Debug, PartialEq)]
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

#[derive(Debug, Clone)]
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

    fn display(&self, position: &Position) {
        let height = self.tiles.len();
        let width = self.tiles[0].len();

        for row in 0..height {
            for col in 0..width {
                if row == position.row && col == position.col {
                    print!("@")
                } else {
                    let tile = self.tiles[row][col];
                    match tile {
                        Tile::OpenPassage => print!("."),
                        Tile::Door(label ) => print!("{}", label.to_ascii_uppercase()),
                        Tile::Key(label ) => print!("{}", label.to_ascii_lowercase()),
                        Tile::Start => print!("@"),
                        Tile::StoneWall => print!("#"),
                    }
                }

            }
            println!()
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
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
        map,
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
        let expected = 8u64;

        assert_eq!(actual, expected)
    }
}