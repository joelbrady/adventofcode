use itertools::Itertools;
use nom::bytes::complete::tag;
use nom::character::complete::{i64, line_ending};
use nom::IResult;
use nom::multi::separated_list1;
use nom::sequence::separated_pair;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    lines: Vec<Line>,
}

#[derive(Debug)]
struct Line {
    begin_x: i64,
    begin_y: i64,
    end_x: i64,
    end_y: i64,
}

impl Line {
    fn all_points(&self) -> Vec<(i64, i64)> {
        let dx = self.end_x - self.begin_x;
        let dx = dx.clamp(-1, 1);

        let dy = self.end_y - self.begin_y;
        let dy = dy.clamp(-1, 1);

        let mut v = vec![];

        let mut x = self.begin_x;
        let mut y = self.begin_y;
        while (x, y) != (self.end_x, self.end_y) {
            v.push((x, y));
            x += dx;
            y += dy;
        }
        v.push((x, y));
        v
    }
}

fn parse_input(s: &str) -> Input {
    let (_, lines) = separated_list1(line_ending, parse_line)(s).unwrap();

    Input {
        lines
    }
}

fn parse_line(s: &str) -> IResult<&str, Line> {
    let (r, ((begin_x, begin_y), (end_x, end_y))) = separated_pair(
        parse_endpoint,
        tag(" -> "),
        parse_endpoint,
    )(s)?;

    Ok((r, Line { begin_x, begin_y, end_x, end_y }))
}

fn parse_endpoint(s: &str) -> IResult<&str, (i64, i64)> {
    separated_pair(i64, tag(","), i64)(s)
}

fn solve(input: &Input) -> usize {
    input.lines.iter()
        .filter(|l| l.begin_x == l.end_x || l.begin_y == l.end_y)
        .flat_map(|l| l.all_points())
        .counts()
        .iter()
        .filter(|(_, count)| **count >= 2)
        .count()
}

fn solve2(input: &Input) -> usize {
    input.lines.iter()
        .flat_map(|l| l.all_points())
        .counts()
        .iter()
        .filter(|(_, count)| **count >= 2)
        .count()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_all_points() {
        let line = Line {
            begin_x: 0,
            begin_y: 9,
            end_x: 5,
            end_y: 9,
        };

        let expected = vec![
            (0, 9),
            (1, 9),
            (2, 9),
            (3, 9),
            (4, 9),
            (5, 9),
        ];

        let actual = line.all_points();

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_all_points2() {
        let line = Line {
            begin_x: 3,
            begin_y: 4,
            end_x: 1,
            end_y: 4,
        };

        let expected = vec![
            (3, 4),
            (2, 4),
            (1, 4)
        ];

        let actual = line.all_points();

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_all_points3() {
        let line = Line {
            begin_x: 0,
            begin_y: 0,
            end_x: 2,
            end_y: 2,
        };

        let expected = vec![
            (0, 0),
            (1, 1),
            (2, 2),
        ];

        let actual = line.all_points();

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 5;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 8350;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 12;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 19374;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}