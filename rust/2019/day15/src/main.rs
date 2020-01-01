use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

use input::get_input;
use intcode::{Machine, parse_program, StoppedState};
use termion::input::TermRead;
use termion::event::Key;
use termion::raw::IntoRawMode;
use std::process::exit;
use std::cmp::{min, max};

fn main() {
    println!("The solution to part 1 is {}", solve_part1());
}

fn solve_part1() -> u64 {
    let input = get_input("2019/day15/input");
    let program = parse_program(&input);
    let mut droid = IntCodePoweredRepairDroid::new(&program);
    let map = explore_entire_map(&mut droid);
    unimplemented!()
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

    fn surrounding(&self, coords: &Coordinates) -> Vec<(Coordinates, Tile)> {
        Movement::values()
            .iter()
            .map(|m| coords.go(*m))
            .map(|c| {
                let tile = self.get(&c);
                (c, tile)
            })
            .collect()
    }

    fn insert(&mut self, coords: Coordinates, tile: Tile) {
        self.map.insert(coords, tile);
        let Coordinates(x, y) = coords;
        self.min_x = min(x, self.min_x);
        self.min_y = min(y, self.min_y);
        self.max_x = max(x, self.max_x);
        self.max_y = max(y, self.max_y);
    }

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

fn explore_entire_map<D>(droid: &mut D) -> (Map, Coordinates)
    where D: RepairDroid {
    let mut current_location: Coordinates = Coordinates(0, 0);
    let mut map = Map::new();
    map.insert(current_location, Tile::Ground);

    loop {
        map.display(&current_location);

        let step = get_keyboard_movement().unwrap_or_else(|| {
            exit(0);
        });

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

    unimplemented!()
}

fn get_keyboard_movement() -> Option<Movement> {
    use Movement::*;

    let mut result = None::<Movement>;

    let stdout = std::io::stdout().into_raw_mode().unwrap();

    for c in std::io::stdin().keys() {
        match c.unwrap() {
            Key::Char('w') => {
                result = Some(North);
                break
            },
            Key::Char('s') => {
                result = Some(South);
                break
            },
            Key::Char('d') => {
                result = Some(East);
                break
            },
            Key::Char('a') => {
                result = Some(West);
                break
            },
            Key::Char('q') => {
                result = None;
                break
            },
            _ => {}
        }
    }

    stdout.suspend_raw_mode().unwrap();

    result
}

fn find_closest_tile(map: &Map, current_location: Coordinates, tile: Tile) -> Option<Vec<Movement>> {
    let mut seen: HashSet<Coordinates> = HashSet::new();
    let mut queue: VecDeque<Rc<Node>> = VecDeque::new();
    queue.push_back(Rc::new(Node { parent: None, coords: current_location, movement: None }));

    while !queue.is_empty() {
        let current = queue.pop_front().unwrap();
        if seen.contains(&current.coords) {
            continue;
        }
        let tile = map.get(&current.coords);
        if tile == Tile::Unexplored {
            return Some(current.build_path());
        }
        seen.insert(current.coords);
        Movement::values()
            .iter()
            .map(|m| (*m, current_location.go(*m)))
            .filter(|(_, coords)| !current.path_contains(coords))
            .filter(|(_, coords)| !seen.contains(coords))
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

#[derive(Debug)]
struct Node {
    coords: Coordinates,
    parent: Option<Rc<Node>>,
    movement: Option<Movement>,
}

impl Node {
    fn path_contains(&self, coords: &Coordinates) -> bool {
        if self.coords == *coords {
            true
        } else {
            match &self.parent {
                Some(node) => node.path_contains(coords),
                None => false,
            }
        }
    }

    fn build_path(&self) -> Vec<Movement> {
        match self.movement {
            Some(m) => {
                let mut v = vec![m];
                let parent = self.parent.as_ref().unwrap();
                let mut v2 = parent.build_path();
                v.append(&mut v2);
                v
            }
            None => vec![],
        }
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
