use std::collections::HashSet;
use std::ops::RangeInclusive;

use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::IResult;
use nom::multi::separated_list1;
use nom::sequence::separated_pair;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    lines: Vec<Line>,
}

#[derive(Debug)]
struct Line {
    segments: Vec<Segment>,
}

impl Line {
    fn contains(&self, v: &(i64, i64)) -> bool {
        self.segments.iter()
            .any(|segment| segment.contains(v))
    }
}

#[derive(Debug)]
enum Segment {
    Horizontal(HorizontalSegment),
    Vertical(VerticalSegment),
}

impl Segment {
    fn contains(&self, (x, y): &(i64, i64)) -> bool {
        match self {
            Segment::Horizontal(hs) => hs.y == *y && hs.x.contains(x),
            Segment::Vertical(vs) => vs.x == *x && vs.y.contains(y),
        }
    }

    fn min_bounds(&self) -> (i64, i64) {
        match self {
            Segment::Horizontal(hs) => (*hs.x.start(), hs.y),
            Segment::Vertical(vs) => (vs.x, *vs.y.start()),
        }
    }

    fn max_bounds(&self) -> (i64, i64) {
        match self {
            Segment::Horizontal(hs) => (*hs.x.end(), hs.y),
            Segment::Vertical(vs) => (vs.x, *vs.y.end()),
        }
    }
}

impl Segment {
    fn from_endpoints((ax, ay): (i64, i64), (bx, by): (i64, i64)) -> Self {
        if ax == bx {
            let min = ay.min(by);
            let max = ay.max(by);
            let s = VerticalSegment { x: ax, y: min..=max };

            Segment::Vertical(s)
        } else {
            let min = ax.min(bx);
            let max = ax.max(bx);
            let s = HorizontalSegment { y: ay, x: min..=max };

            Segment::Horizontal(s)
        }
    }
}

#[derive(Debug)]
struct HorizontalSegment {
    y: i64,
    x: RangeInclusive<i64>,
}

#[derive(Debug)]
struct VerticalSegment {
    y: RangeInclusive<i64>,
    x: i64,
}

fn parse_input(s: &str) -> Input {
    let (_, lines) = separated_list1(line_ending, parse_line)(s).unwrap();

    Input { lines}
}

fn parse_line(s: &str) -> IResult<&str, Line> {
    let p = nom::character::complete::i64;
    let (rem, ts) = separated_list1(tag(" -> "), separated_pair(p, tag(","), p))(s)?;

    let segments = ts.as_slice()
        .windows(2)
        .map(|window| {
            assert_eq!(window.len(), 2);
            let a = window[0];
            let b = window[1];
            Segment::from_endpoints(a, b)
        })
        .collect();

    Ok((rem, Line { segments }))
}

struct Rocks<'a> {
    lines: &'a [Line],
}

impl <'a> Rocks<'a> {
    fn contains(&'a self, v: &(i64, i64)) -> bool {
        self.lines.iter()
            .any(|line| line.contains(v))
    }

    fn in_bounds(&'a self, (x, y): &(i64, i64)) -> bool {
        let (min_x, _) = self.min_bounds();
        let (max_x, max_y) = self.max_bounds();

        let x = *x;
        let y = *y;

        !(x < min_x || x > max_x || y > max_y)
    }

    fn min_bounds(&self) -> (i64, i64) {
        self.lines.iter()
            .flat_map(|l| l.segments.iter())
            .map(|s| s.min_bounds())
            .reduce(|(ax, ay), (bx, by)| (ax.min(bx), ay.min(by)))
            .unwrap()
    }

    fn max_bounds(&self) -> (i64, i64) {
        self.lines.iter()
            .flat_map(|l| l.segments.iter())
            .map(|s| s.max_bounds())
            .reduce(|(ax, ay), (bx, by)| (ax.max(bx), ay.max(by)))
            .unwrap()
    }
}

fn solve_part1(input: &Input) -> i64 {
    let rocks = Rocks { lines: &input.lines };
    let mut sand_at_rest = HashSet::new();

    loop {
        let mut prev = (500, -1);
        let mut current = (500, 0);


        while prev != current && rocks.in_bounds(&current) {
            // print(&rocks, &sand_at_rest, current);

            let dirs = [(0, 1), (-1, 1), (1, 1)];

            let mut moved = false;
            for (dx, dy) in dirs {
                let (x, y) = current;
                let (x, y) = (x + dx, y + dy);

                if !rocks.contains(&(x, y)) && !sand_at_rest.contains(&(x, y)) {
                    prev = current;
                    current = (x, y);
                    moved = true;
                    break;
                }
            }

            if !moved {
                prev = current;
            }
        }

        if prev == current {
            sand_at_rest.insert(current);
        } else {
            return sand_at_rest.len() as i64;
        }
    }
}

#[allow(dead_code)]
fn print(rocks: &Rocks, sand_at_rest: &HashSet<(i64, i64)>, moving_sand: (i64, i64)) {
    let (min_x, _) = rocks.min_bounds();
    let (max_x, max_y) = rocks.max_bounds();
    for y in -1..(max_y + 3) {
        for x in (min_x - 3)..=(max_x + 3) {
            let v = (x, y);
            if v == moving_sand || sand_at_rest.contains(&v) {
                print!("O");
            } else if rocks.contains(&v) {
                print!("#");
            } else if v == (500, 0) {
                print!("+");
            } else {
                print!(".");
            }
        }
        println!();
    }
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 24;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 805;
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