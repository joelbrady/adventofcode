use std::cmp::{max, min};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

use crate::cal2019::intcode::{Machine, parse_program, StoppedState};

pub fn main() {
    println!("The solution to part 1 is {}", solve_part1());
    println!("The solution to part 2 is {}", solve_part2());
}

fn solve_part1() -> u64 {
    let input = include_str!("input");
    let program = parse_program(&input);
    let mut droid = IntCodePoweredRepairDroid::new(&program);
    let map = explore_entire_map(&mut droid);
    let path_to_oxygen = get_path_to_oxygen(&map);
    path_to_oxygen.len() as u64
}

fn solve_part2() -> u64 {
    let input = include_str!("input");
    let program = parse_program(&input);
    let mut droid = IntCodePoweredRepairDroid::new(&program);
    let map = explore_entire_map(&mut droid);
    let path_to_oxygen = get_path_to_oxygen(&map);
    let oxygen_location = path_to_oxygen.iter().fold(Coordinates(0, 0), |c, m| c.go(*m));
    find_longest_path_length(&map, &oxygen_location) - 1
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
struct Coordinates(i64, i64);

impl From<(i64, i64)> for Coordinates {
    fn from(tuple: (i64, i64)) -> Self {
        let (x, y) = tuple;
        Coordinates(x, y)
    }
}

impl Coordinates {
    fn go(&self, movement: Movement) -> Coordinates {
        use Movement::*;

        let (x, y) = (self.0, self.1);
        match movement {
            North => (x, y - 1),
            South => (x, y + 1),
            West => (x - 1, y),
            East => (x + 1, y),
        }.into()
    }
}

#[derive(Debug)]
struct Map {
    map: HashMap<Coordinates, Tile>,
    min_x: i64,
    min_y: i64,
    max_x: i64,
    max_y: i64,
}

impl Map {
    fn new() -> Map {
        Map {
            map: HashMap::new(),
            min_x: -1,
            min_y: -1,
            max_x: 1,
            max_y: 1,
        }
    }

    fn get(&self, coords: &Coordinates) -> Tile {
        *(self.map.get(coords).unwrap_or(&Tile::Unexplored))
    }

    fn insert(&mut self, coords: Coordinates, tile: Tile) {
        self.map.insert(coords, tile);

        // update bounds for nice drawing later
        let Coordinates(x, y) = coords;
        self.min_x = min(x, self.min_x);
        self.min_y = min(y, self.min_y);
        self.max_x = max(x, self.max_x);
        self.max_y = max(y, self.max_y);
    }

    #[allow(dead_code)]
    fn display(&self, droid_location: &Coordinates) {
        for y in (self.min_y - 1)..=(self.max_y + 1) {
            for x in (self.min_x - 1)..=(self.max_x + 1) {
                let coords = Coordinates(x, y);
                if coords == *droid_location {
                    print!("D");
                } else {
                    let tile = self.get(&coords);
                    print!("{}", tile.as_str());
                }
            }
            println!();
        }
    }
}

fn explore_entire_map<D>(droid: &mut D) -> Map
    where D: RepairDroid {
    let mut current_location: Coordinates = Coordinates(0, 0);
    let mut map = Map::new();
    map.insert(current_location, Tile::Ground);

    while let Some(path) = find_closest_tile(&map, &current_location, Tile::Unexplored) {
        for step in path {
            match droid.go(step) {
                MovementResult::HitWall => {
                    let target_location = current_location.go(step);
                    map.insert(target_location, Tile::Wall);
                }
                MovementResult::Success => {
                    current_location = current_location.go(step);
                    map.insert(current_location, Tile::Ground);
                }
                MovementResult::FoundOxygen => {
                    current_location = current_location.go(step);
                    map.insert(current_location, Tile::OxygenSystem)
                }
            }
        }
    }

    map
}

fn get_path_to_oxygen(map: &Map) -> Vec<Movement> {
    find_closest_tile(map, &Coordinates(0, 0), Tile::OxygenSystem).unwrap()
}

fn find_closest_tile(map: &Map, current_location: &Coordinates, tile_type: Tile) -> Option<Vec<Movement>> {
    let mut seen: HashSet<Coordinates> = HashSet::new();
    let mut queue: VecDeque<Rc<Node>> = VecDeque::new();
    queue.push_back(Rc::new(Node { parent: None, coords: *current_location, movement: None }));

    while let Some(current) = queue.pop_front() {
        if !seen.insert(current.coords) {
            continue;
        }
        let tile = map.get(&current.coords);
        if tile == tile_type {
            let path = current.build_path();
            return Some(path);
        }
        if tile == Tile::Wall {
            continue;
        }
        Movement::values()
            .iter()
            .map(|m| (*m, current.coords.go(*m)))
            .for_each(|(movement, coords)| {
                let node = Rc::new(Node {
                    coords,
                    parent: Some(current.clone()),
                    movement: Some(movement),
                });
                queue.push_back(node);
            });
    }

    None
}

fn find_longest_path_length(map: &Map, start: &Coordinates) -> u64 {
    let mut seen: HashSet<Coordinates> = HashSet::new();
    let mut queue: VecDeque<Rc<Node>> = VecDeque::new();
    let mut most_recent = Rc::new(Node { parent: None, coords: *start, movement: None });
    queue.push_back(Rc::clone(&most_recent));

    while let Some(current) = queue.pop_front() {
        if !seen.insert(current.coords) {
            continue;
        } else {
            most_recent = Rc::clone(&current);
        }
        let tile = map.get(&current.coords);
        if tile == Tile::Wall {
            continue;
        }
        Movement::values()
            .iter()
            .map(|m| (*m, current.coords.go(*m)))
            .for_each(|(movement, coords)| {
                let node = Rc::new(Node {
                    coords,
                    parent: Some(Rc::clone(&current)),
                    movement: Some(movement),
                });
                queue.push_back(node);
            });
    }

    most_recent.build_path().len() as u64
}

#[derive(Debug)]
struct Node {
    coords: Coordinates,
    parent: Option<Rc<Node>>,
    movement: Option<Movement>,
}

impl Node {
    fn build_path(&self) -> Vec<Movement> {
        self.movement.and_then(|m|
            self.parent.as_ref()
                .map(|p| {
                    let mut a = p.build_path();
                    a.push(m);
                    a
                })
        )
            .unwrap_or_else(Vec::new)
    }
}

trait RepairDroid {
    fn go(&mut self, movement: Movement) -> MovementResult;
}

enum MovementResult {
    HitWall,
    Success,
    FoundOxygen,
}

impl From<i64> for MovementResult {
    fn from(int: i64) -> Self {
        match int {
            0 => MovementResult::HitWall,
            1 => MovementResult::Success,
            2 => MovementResult::FoundOxygen,
            _ => panic!("unexpected movement result"),
        }
    }
}

struct IntCodePoweredRepairDroid {
    brain: Machine,
}

impl IntCodePoweredRepairDroid {
    fn new(program: &[i64]) -> Self {
        let mut brain = Machine::new_feedback_mode(program);
        if let StoppedState::BlockedOnInput = brain.run() {
            return IntCodePoweredRepairDroid { brain };
        }
        panic!("brain initialisation failed")
    }
}

impl RepairDroid for IntCodePoweredRepairDroid {
    fn go(&mut self, movement: Movement) -> MovementResult {
        self.brain.input(movement.as_int());
        if let StoppedState::Halted = self.brain.run() {
            panic!("intcode program halted unexpectedly")
        }

        let result = self.brain.output();
        MovementResult::from(result)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Movement {
    North,
    East,
    South,
    West,
}

impl Movement {
    fn as_int(self) -> i64 {
        use Movement::*;

        match self {
            North => 1,
            South => 2,
            West => 3,
            East => 4,
        }
    }

    fn values() -> Vec<Movement> {
        use Movement::*;

        vec![North, South, East, West]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Unexplored,
    Ground,
    Wall,
    OxygenSystem,
}

impl Tile {
    fn as_str(self) -> &'static str {
        use Tile::*;

        match self {
            Unexplored => " ",
            Ground => ".",
            Wall => "#",
            OxygenSystem => "O",
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        assert_eq!(solve_part1(), 380);
        assert_eq!(solve_part2(), 410);
    }
}