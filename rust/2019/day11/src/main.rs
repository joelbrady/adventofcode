use std::collections::HashMap;
use std::ops::Add;

use input::get_input;
use intcode::{Machine, parse_program, StoppedState};
use std::cmp::{min, max};

fn main() {
    let input = get_input("input");
    let program = parse_program(&input);

    println!("The solution to part 1 is {}", part1(&program));

    part2(&program);
}

fn part1(program: &[i64]) -> usize {
    let mut hull = Hull::new(&Color::Black);
    let mut robot = Robot::new(&program);

    loop {
        let current_color = hull.get_current_color();
        let output = robot.give_input(&current_color);
        match output {
            State::Halted => break,
            State::Running((color, turn)) => {
                hull.paint(&color);
                robot.turn(&turn);
                hull.move_forward(&robot.direction)
            },
        }
    }

    hull.panels_painted()
}

fn part2(program: &[i64]) {
    let mut hull = Hull::new(&Color::White);
    let mut robot = Robot::new(&program);

    loop {
        let current_color = hull.get_current_color();
        let output = robot.give_input(&current_color);
        match output {
            State::Halted => break,
            State::Running((color, turn)) => {
                hull.paint(&color);
                robot.turn(&turn);
                hull.move_forward(&robot.direction)
            },
        }
    }

    let mut min_x: i64 = 0;
    let mut min_y: i64 = 0;
    let mut max_x: i64 = 0;
    let mut max_y: i64 = 0;
    for (position, _) in &hull.painted {
        let (x, y) = position.as_tuple();
        min_x = min(x, min_x);
        min_y = min(y, min_y);
        max_x = max(x, max_x);
        max_y = max(y, max_y);
    }

    for y in min_y..max_y+1 {
        for x in min_x..max_x+1 {
            let color = hull.painted.get(&Position::from((x, y))).unwrap_or(&Color::Black);
            match color {
                Color::White => print!("#"),
                Color::Black => print!(" "),
            };
        }
        println!();
    }

}

#[derive(Copy, Clone, Debug)]
enum Color {
    Black,
    White,
}

enum Turn {
    Left,
    Right,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
struct Position {
    x: i64,
    y: i64,
}

impl Position {
    fn as_tuple(&self) -> (i64, i64) {
        (self.x, self.y)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn to_vector(&self) -> (i64, i64) {
        use Direction::*;

        match self {
            Up => (0, -1),
            Down => (0, 1),
            Left => (-1, 0),
            Right => (1, 0),
        }
    }
}

impl From<(i64, i64)> for Position {
    fn from(tuple: (i64, i64)) -> Self {
        let (x, y) = tuple;
        Position { x, y }
    }
}

impl Add<Position> for Position {
    type Output = Self;

    fn add(self, rhs: Position) -> Self::Output {
        Position { x: self.x + rhs.x, y: self.y + rhs.y }
    }
}

impl Add<(i64, i64)> for Position {
    type Output = Self;

    fn add(self, rhs: (i64, i64)) -> Self::Output {
        let (x, y) = rhs;
        Position { x: self.x + x, y: self.y + y }
    }
}

#[derive(Debug)]
struct Hull {
    painted: HashMap<Position, Color>,
    robot_position: Position,
}

impl Hull {
    fn new(start_color: &Color) -> Hull {
        let mut hull = Hull { painted: HashMap::new(), robot_position: (0, 0).into()};
        hull.paint(start_color);
        hull
    }

    fn get_current_color(&self) -> Color {
        self.painted.get(&self.robot_position)
            .map_or(Color::Black, |color_ref| *color_ref)
    }

    fn paint(&mut self, color: &Color) {
        self.painted.insert(self.robot_position, *color);
    }

    fn move_forward(&mut self, direction: &Direction) {
        self.robot_position = self.robot_position + direction.to_vector();
    }

    fn panels_painted(&self) -> usize {
        self.painted.iter()
            .count()
    }
}

struct Robot {
    brain: Machine,
    direction: Direction,
}

impl Robot {
    fn new(program: &[i64]) -> Robot {
        let brain = Machine::new_feedback_mode(program);
        Robot { brain, direction: Direction::Up }
    }

    fn give_input(&mut self, input: &Color) -> State {
        use Color::*;

        let color_as_int = match input {
            Black => 0,
            White => 1,
        };

        self.brain.input(color_as_int);

        let state = self.brain.run();

        if let StoppedState::Halted = state {
            return State::Halted
        }

        let color = match self.brain.output() {
            0 => Black,
            1 => White,
            _ => unimplemented!("unknown color"),
        };

        let turn = match self.brain.output() {
            0 => Turn::Left,
            1 => Turn::Right,
            _ => unimplemented!("unknown turn"),
        };
        State::Running((color, turn))
    }

    fn turn(&mut self, turn: &Turn) {
        use Direction::*;

        self.direction = match turn {
            Turn::Left => match self.direction {
                Up => Left,
                Left => Down,
                Down => Right,
                Right => Up,
            },
            Turn::Right => match self.direction {
                Up => Right,
                Right => Down,
                Down => Left,
                Left => Up,
            }
        }
    }
}

enum State {
    Running((Color, Turn)),
    Halted
}
