use std::collections::HashSet;

use nom::{IResult, Parser};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, multispace1};
use nom::multi::separated_list1;
use nom::sequence::separated_pair;

const DEBUG: bool = false;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Eq, PartialEq)]
struct Input {
    instructions: Vec<Instruction>,
}

#[derive(Debug, Eq, PartialEq)]
struct Instruction {
    dir: V,
    amount: i32,
}

fn parse_input(s: &str) -> Input {
    let (_, instructions) = separated_list1(line_ending, parse_instruction)(s).unwrap();

    Input {
        instructions
    }
}

fn parse_instruction(s: &str) -> IResult<&str, Instruction> {
    let (rem, (dir, amount)) = separated_pair(
        parse_direction,
        multispace1,
        nom::character::complete::i32,
    )(s)?;

    let instruction = Instruction { dir, amount };

    Ok((rem, instruction))
}

fn parse_direction(s: &str) -> IResult<&str, V> {
    alt((
        tag("U").map(|_| (0, 1)),
        tag("D").map(|_| (0, -1)),
        tag("L").map(|_| (-1, 0)),
        tag("R").map(|_| (1, 0)),
    ))(s)
        .map(|(rem, (x, y))| (rem, V::new(x, y)))
}

fn solve_part1(input: &Input) -> i32 {
    let mut head = V::new(0, 0);
    let mut tail = V::new(0, 0);

    let mut tail_visited = HashSet::new();
    tail_visited.insert(tail);

    print_world(head, tail, &tail_visited);

    for i in &input.instructions {
        for _ in 0..i.amount {
            head = head + i.dir;

            if tail.is_touching(head) {
                // touching
            } else if (tail.x == head.x || tail.y == head.y) && head.dist(tail) > 0 {
                // same col or row
                let dir = tail.direction_to(head);
                let dir = dir.normalize();
                tail = tail + dir;
                tail_visited.insert(tail);
            } else {
                // diagonal
                let mut dir = V::new(0, 0);
                if head.x > tail.x {
                    dir = dir + V::new(1, 0);
                } else {
                    dir = dir + V::new(-1, 0);
                }

                if head.y > tail.y {
                    dir = dir + V::new(0, 1);
                } else {
                    dir = dir + V::new(0, -1);
                }

                tail = tail + dir;
                tail_visited.insert(tail);
            }

            print_world(head, tail, &tail_visited);
        }
    }

    tail_visited.len() as i32
}

fn print_world(head: V, tail: V, tail_visited: &HashSet<V>) {
    if DEBUG {
        for y in (0..5).rev() {
            for x in 0..5 {
                let c = V::new(x, y);
                if c == head {
                    print!("H");
                } else if c == tail {
                    print!("T");
                } else if c == V::new(0, 0) {
                    print!("s");
                } else if tail_visited.contains(&c) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
struct V {
    x: i32,
    y: i32,
}

impl V {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    fn dist(self, other: Self) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }

    fn direction_to(self, other: Self) -> Self {
        other - self
    }

    fn normalize(self) -> Self {
        let length = if self.x != 0 && self.y != 0 {
            panic!("only support length of cardinal vector")
        } else if self.x != 0 {
            self.x
        } else {
            self.y
        };

        let length = length.abs();

        V::new(self.x / length, self.y / length)
    }

    fn is_touching(self, target: Self) -> bool {
        let around = [
            (0, 0),  // on top
            (1, 0),  // right
            (1, 1),  // up-right
            (1, -1), // down-right
            (-1, 1), // up-left
            (-1, 0), // left
            (-1, -1),// down-left
            (0, 1),  // up
            (0, -1), // down
        ];

        for (dx, dy) in around {
            let v = V::new(target.x + dx, target.y + dy);
            if v == self {
                return true;
            }
        }

        false
    }
}

impl std::ops::Add for V {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl std::ops::Sub for V {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        V::new(self.x - rhs.x, self.y - rhs.y)
    }
}

fn solve_part2(_input: &Input) -> i32 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 13;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 6332;
        let actual = solve_part1(&input);

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