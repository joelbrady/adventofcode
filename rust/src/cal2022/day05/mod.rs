use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::line_ending;
use nom::combinator::map;
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

#[derive(Debug, Eq, PartialEq)]
struct Input {
    stacks: Vec<Stack>,
    moves: Vec<Move>,
}

#[derive(Debug, Eq, PartialEq)]
struct Stack {}

#[derive(Debug, Eq, PartialEq)]
struct Move {}

fn parse_input(s: &str) -> Input {
    let (_, (stacks, moves)) = separated_pair(
        parse_stacks,
        line_ending,
        parse_moves)
        (s).unwrap();

    Input {
        stacks,
        moves,
    }
}

fn parse_moves(s: &str) -> IResult<&str, Vec<Move>> {
    todo!()
}

fn parse_stacks(s: &str) -> IResult<&str, Vec<Stack>> {
    let (rem, rows) = separated_list1(line_ending, parse_row)(s)?;

    dbg!(rows);

    todo!()
}

#[derive(Debug, Eq, PartialEq)]
enum MaybeCrate {
    Empty,
    Crate(Crate),
}

fn parse_maybe_crate(s: &str) -> IResult<&str, MaybeCrate> {
    alt((
        map(tag("   "), |_| MaybeCrate::Empty),
        map(parse_crate, MaybeCrate::Crate),
    ))(s)
}

#[derive(Debug, Eq, PartialEq)]
struct Row {
    cols: Vec<MaybeCrate>,
}

fn parse_row(s: &str) -> IResult<&str, Row> {
    let (rem, cols) = separated_list1(tag(" "), parse_maybe_crate)(s)?;
    Ok((rem, Row { cols }))
}

#[derive(Debug, Eq, PartialEq)]
struct Crate {
    label: String,
}

fn parse_crate(s: &str) -> IResult<&str, Crate> {
    let (rem, _) = tag("[")(s)?;
    let (rem, label) = is_not("]")(rem)?;
    let (rem, _) = tag("]")(rem)?;

    let label = label.to_owned();

    Ok((rem, Crate { label }))
}

fn solve_part1(input: &Input) -> i64 {
    dbg!(&input);
    todo!()
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_row() {
        let expected = Row {
            cols: vec![
                MaybeCrate::Crate(Crate {
                    label: "Z".into(),
                }),
                MaybeCrate::Crate(Crate {
                    label: "M".into(),
                }),
                MaybeCrate::Crate(Crate {
                    label: "P".into(),
                }),
            ]
        };
        let actual = parse_row("[Z] [M] [P]");

        assert_eq!(actual, Ok(("", expected)))
    }

    #[test]
    fn test_parse_row2() {
        let expected = Row {
            cols: vec![
                MaybeCrate::Empty,
                MaybeCrate::Crate(Crate {
                    label: "D".into(),
                }),
                MaybeCrate::Empty,
            ]
        };
        let actual = parse_row("    [D]    ");

        assert_eq!(actual, Ok(("", expected)))
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 157;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 8053;
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