use std::collections::HashSet;

use nom::character::complete::{anychar, char, digit1, multispace0};
use nom::combinator::opt;
use nom::IResult;
use std::io::{stdin, Read};

fn main() {
    let input = get_input();
    let input = parse(&input);
    let solution = solve(input);

    println!("The solution to part 1 is {}", solution);
}

fn get_input() -> String {
    let mut string = String::new();
    stdin().lock()
        .read_to_string(&mut string)
        .unwrap();
    string
}

fn parse(s: &str) -> Input {
    let wires: Vec<&str> = s
        .split("\n")
        .collect();

    let a = parse_wire(wires[0]);
    let b = parse_wire(wires[1]);

    Input { a, b }
}

fn parse_wire(s: &str) -> Wire {
    let path: Vec<Line> = s
        .split(",")
        .map(|w| {
            let (_, line) = parse_line(w).unwrap();
            line
        })
        .collect();

    Wire { path }
}

fn parse_line(input: &str) -> IResult<&str, Line> {
    let (input, dir) = parse_direction(input)?;
    let (input, dist) = parse_i32(input)?;

    Ok((input, Line { direction: dir, distance: dist }))
}

fn parse_i32(input: &str) -> IResult<&str, i32> {
    let (input, _) = multispace0(input)?;
    let (input, sign) = opt(char('-'))(input)?;
    let (input, digits) = digit1(input)?;
    let n: i32 = digits.parse().unwrap();
    let n: i32 = match sign {
        Some(_) => -1 * n,
        None => n
    };
    Ok((input, n))
}

fn parse_direction(s: &str) -> IResult<&str, Direction> {
    use Direction::*;

    let (input, c) = anychar(s)?;

    let d = match c {
        'U' => Up,
        'D' => Down,
        'L' => Left,
        'R' => Right,
        _ => unimplemented!()
    };

    Ok((input, d))
}

#[derive(Debug, Eq, PartialEq)]
struct Input {
    a: Wire,
    b: Wire,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Wire {
    path: Vec<Line>
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct Line {
    direction: Direction,
    distance: i32,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn to_vector(&self) -> (i32, i32) {
        use Direction::*;

        match self {
            Up => (0, -1),
            Down => (0, 1),
            Left => (-1, 0),
            Right => (1, 0),
        }
    }
}

fn solve(input: Input) -> i32 {
    let mut visited_a: HashSet<(i32, i32)> = HashSet::new();
    let (mut x, mut y) = (0, 0);

    for line in input.a.path {
        let (dx, dy) = line.direction.to_vector();
        for _ in 0..line.distance {
            x += dx;
            y += dy;
//            println!("a visited {},{}", x, y);
            visited_a.insert((x, y));
        }
    }

    let mut visited_b: HashSet<(i32, i32)> = HashSet::new();
    let (mut x, mut y) = (0, 0);


    for line in input.b.path {
        let (dx, dy) = line.direction.to_vector();
        for _ in 0..line.distance {
            x += dx;
            y += dy;
//            println!("b visited {},{}", x, y);
            visited_b.insert((x, y));
        }
    }

//    let d: Vec<&(i32, i32)> = visited_a.intersection(&visited_b).collect();
//    println!("{:?}", d);

    let intersection: i32 = visited_a.intersection(&visited_b)
        .map(|(a, b)| manhattan(*a, *b))
        .min()
        .unwrap();

    intersection
}

fn manhattan(x: i32, y: i32) -> i32 {
    abs(x) + abs(y)
}

fn abs(n: i32) -> i32 {
    if n < 0 {
        -n
    } else {
        n
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_direction_to_vector() {
        assert_eq!(Direction::Right.to_vector(), (1, 0))
    }

    #[test]
    fn test_parse() {
        use Direction::*;

        let a = Wire {
            path: vec![
                Line { direction: Right, distance: 8 },
                Line { direction: Up, distance: 5 },
                Line { direction: Left, distance: 5 },
                Line { direction: Down, distance: 3 },
            ],
        };
        let b = Wire {
            path: vec![
                Line { direction: Up, distance: 7 },
                Line { direction: Right, distance: 6 },
                Line { direction: Down, distance: 4 },
                Line { direction: Left, distance: 4 },
            ],
        };
        let expected = Input { a, b };
        assert_eq!(parse("R8,U5,L5,D3\nU7,R6,D4,L4"), expected)
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("U42").unwrap(), ("", Line { direction: Direction::Up, distance: 42 }));
    }

    #[test]
    fn test_parse_wire() {
        use Direction::*;

        let wire = parse_wire("U1,D2,L3,R4");
        let a = Line { direction: Up, distance: 1 };
        let b = Line { direction: Down, distance: 2 };
        let c = Line { direction: Left, distance: 3 };
        let d = Line { direction: Right, distance: 4 };
        let expected = Wire { path: vec![a, b, c, d] };
        assert_eq!(wire, expected);
    }

    #[test]
    fn solve_example() {
        let input = parse("R8,U5,L5,D3\nU7,R6,D4,L4");
        let answer = solve(input);
        assert_eq!(answer, 6)
    }
}
