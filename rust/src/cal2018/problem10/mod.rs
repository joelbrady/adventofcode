use std::collections::HashMap;
use std::fmt::{Error, Formatter};

use nom::bytes::complete::tag;
use nom::character::complete::{char, multispace1};
use nom::IResult;
use nom::sequence::separated_pair;

use crate::parse::parse_i32;

pub fn solve() -> std::io::Result<()> {
    let input = include_str!("input.a");
    let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();

    println!("got input");

    let mut points: Vec<Point> = lines
        .iter()
        .map(|s| parse_point(s.as_str())
            .map(|(_, point)| point))
        .collect::<Result<Vec<Point>, _>>()
        .unwrap();

    println!("parsed points");

    display(&points);

    for i in 0..100000 {
        points = iterate(points);
        println!("{}", i);
        display(&points)
    }

    Ok(())
}

fn iterate(points: Vec<Point>) -> Vec<Point> {
    points.iter()
        .map(|p| p.step())
        .collect()
}

fn display(points: &Vec<Point>) {
    let xs: Vec<i32> = points
        .iter()
        .map(|p| p.position.x)
        .collect();

    let ys: Vec<i32> = points
        .iter()
        .map(|p| p.position.y)
        .collect();

    let min_x = xs.iter().min().unwrap().clone();
    let min_y = ys.iter().min().unwrap().clone();
    let max_x = xs.iter().max().unwrap().clone();
    let max_y = ys.iter().max().unwrap().clone();

    let limit = 100;

    if distance(&min_x, &max_x) > limit {
        return
    }

    if distance(&min_y, &max_y) > limit {
        return
    }

    let mut grid: HashMap<(i32, i32), &str> = HashMap::new();

    for y in min_y..max_y {
        for x in min_x..max_x {
            grid.insert((x, y), ".");
        }
    }

    for p in points {
        let (x, y) = (p.position.x, p.position.y);
        grid.insert((x, y), "#");
    }

    for y in min_y..max_y {
        for x in min_x..max_x {
            print!("{}", grid.get(&(x, y)).unwrap());
        }
        println!();
    }

    println!()
}

fn distance(x: &i32, y: &i32) -> i32 {
    let d = x - y;
    if d < 0 {
        -d
    } else {
        d
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Position {
    x: i32,
    y: i32,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "position=<{}, {}>", self.x, self.y)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct Vector {
    x: i32,
    y: i32,
}

impl std::fmt::Display for Vector {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "velocity=<{}, {}>", self.x, self.y)
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Point {
    position: Position,
    direction: Vector,
}

impl Point {
    fn create(position: Position, direction: Vector) -> Point {
        Point { position, direction }
    }

    fn step(&self) -> Point {
        let x: i32 = self.position.x + self.direction.x;
        let y: i32 = self.position.y + self.direction.y;

        Point::create(Position { x, y }, self.direction)
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        self.position.fmt(f)?;
        write!(f, " ")?;
        self.direction.fmt(f)?;
        Ok(())
    }
}

fn parse_point(input: &str) -> IResult<&str, Point> {
    let (input, position) = parse_position(input)?;
    let (input, _) = multispace1(input)?;
    let (input, velocity) = parse_velocity(input)?;
    Ok((input, Point::create(position, velocity)))
}

fn parse_position(input: &str) -> IResult<&str, Position> {
    let (input, _) = tag("position=")(input)?;
    let (input, (x, y)) = parse_pair(input)?;
    Ok((input, Position { x, y }))
}

fn parse_velocity(input: &str) -> IResult<&str, Vector> {
    let (input, _) = tag("velocity=")(input)?;
    let (input, (x, y)) = parse_pair(input)?;
    Ok((input, Vector { x, y }))
}

fn parse_pair(input: &str) -> IResult<&str, (i32, i32)> {
    let (input, _) = char('<')(input)?;
    let (input, (x, y)) = separated_pair(parse_i32, char(','), parse_i32)(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, (x, y)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_point() {
        let example = "position=< 9,  1> velocity=< 0,  2>";
        let result = parse_point(example);
        let expected = Point::create(Position { x: 9, y: 1 }, Vector { x: 0, y: 2 });
        assert_eq!(result, Ok(("", expected)))
    }

    #[test]
    fn test_parse_pair() {
        let result = parse_pair("< 10, -44>");
        let expected = Ok(("", (10, -44)));
        assert_eq!(result, expected)
    }

    #[test]
    fn solve_test() {
        solve().unwrap()
    }
}
