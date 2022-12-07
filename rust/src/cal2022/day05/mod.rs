use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{digit1, line_ending, space0, space1};
use nom::combinator::map;
use nom::IResult;
use nom::multi::{count, separated_list1};
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
    stacks: Vec<Stack>,
    moves: Vec<Move>,
}

#[derive(Debug, Clone)]
struct Stack {
    label: String,
    crates: Vec<Crate>,
}

#[derive(Debug, Eq, PartialEq)]
struct Move {
    count: u32,
    src: String,
    dst: String,
}

fn parse_input(s: &str) -> Input {
    let (_, (stacks, moves)) = separated_pair(
        parse_stacks,
        count(line_ending, 2),
        parse_moves,
    )(s).unwrap();

    Input {
        stacks,
        moves,
    }
}

fn parse_moves(s: &str) -> IResult<&str, Vec<Move>> {
    separated_list1(line_ending, parse_move)(s)
}

fn parse_move(s: &str) -> IResult<&str, Move> {
    use nom::character::complete::u32 as uint;

    let (rem, _) = tag("move")(s)?;
    let (rem, _) = space1(rem)?;
    let (rem, count) = uint(rem)?;
    let (rem, _) = space1(rem)?;
    let (rem, _) = tag("from")(rem)?;
    let (rem, _) = space1(rem)?;
    let (rem, src) = digit1(rem)?;
    let (rem, _) = space1(rem)?;
    let (rem, _) = tag("to")(rem)?;
    let (rem, _) = space1(rem)?;
    let (rem, dst) = digit1(rem)?;

    Ok((rem, Move {
        count,
        src: src.to_owned(),
        dst: dst.to_owned(),
    }))
}

fn parse_stacks(s: &str) -> IResult<&str, Vec<Stack>> {
    let (rem, rows) = separated_list1(line_ending, parse_row)(s)?;
    let (rem, _) = line_ending(rem)?;
    let (rem, _) = space0(rem)?;
    let (rem, labels) = separated_list1(space1, digit1)(rem)?;
    let (rem, _) = space0(rem)?;

    let labels: Vec<String> = labels.into_iter()
        .map(|s| s.to_owned())
        .collect();

    let mut stacks: Vec<Stack> = labels.iter()
        .map(|label| Stack {
            label: label.clone(),
            crates: vec![],
        })
        .collect();

    assert!(!rows.is_empty());
    assert_eq!(rows[0].cols.len(), labels.len());

    for row in &rows {
        for (i, maybe_crate) in row.cols.iter().enumerate() {
            if let MaybeCrate::Crate(c) = maybe_crate {
                let stack = &mut stacks[i];
                stack.crates.push(c.clone());
            }
        }
    }

    for stack in stacks.iter_mut() {
        stack.crates.reverse();
    }

    Ok((rem, stacks))
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

#[derive(Debug, Eq, PartialEq, Clone)]
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

fn solve_part1(input: &Input) -> String {
    let mut stacks = input.stacks.clone();

    for m in &input.moves {
        for _ in 0..m.count {
            let src = get_stack_by_label(&mut stacks, &m.src);
            let c = src.crates.pop().unwrap();
            let dst = get_stack_by_label(&mut stacks, &m.dst);
            dst.crates.push(c);
        }
    }

    stacks.iter()
        .map(|s| s.crates.last().unwrap())
        .map(|c| c.label.as_str())
        .collect()
}

fn get_stack_by_label<'a>(stacks: &'a mut [Stack], label: &str) -> &'a mut Stack {
    for s in stacks {
        if s.label == label {
            return s;
        }
    }

    panic!()
}

fn solve_part2(input: &Input) -> String {
    let mut stacks = input.stacks.clone();

    for m in &input.moves {
        let mut buffer = vec![];

        let src = get_stack_by_label(&mut stacks, &m.src);
        for _ in 0..m.count {
            let c = src.crates.pop().unwrap();
            buffer.push(c);
        }

        buffer.reverse();

        let dst = get_stack_by_label(&mut stacks, &m.dst);
        for c in buffer {
            dst.crates.push(c);
        }
    }

    stacks.iter()
        .map(|s| s.crates.last().unwrap())
        .map(|c| c.label.as_str())
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_move() {
        let input = "move 1 from 2 to 1";
        let expected = Move {
            dst: "1".into(),
            src: "2".into(),
            count: 1,
        };
        let actual = parse_move(input);

        assert_eq!(actual, Ok(("", expected)))
    }

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
        let expected = "CMZ";
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = "JCMHLVGMG";
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = "MCD";
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = "LVMRWSSPZ";
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}