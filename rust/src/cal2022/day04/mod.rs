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

#[derive(Debug, Eq, PartialEq)]
struct Input {
    pairs: Vec<Pair>,
}

#[derive(Debug, Eq, PartialEq)]
struct Pair {
    first: RangeInclusive<u32>,
    second: RangeInclusive<u32>,
}

fn parse_input(s: &str) -> Input {
    let (_, pairs) = separated_list1(line_ending, parse_pair)(s).unwrap();
    Input { pairs }
}

fn parse_pair(s: &str) -> IResult<&str, Pair> {
    let (rem, (first, second)) = separated_pair(
        parse_range,
        tag(","),
        parse_range,
    )(s)?;

    let pair = Pair {
        first,
        second,
    };

    Ok((rem, pair))
}

fn parse_range(s: &str) -> IResult<&str, RangeInclusive<u32>> {
    use nom::character::complete::u32 as uint_parser;

    let (rem, (start, end)) = separated_pair(uint_parser, tag("-"), uint_parser)(s)?;

    Ok((rem, start..=end))
}

fn solve_part1(input: &Input) -> usize {
    input.pairs.iter()
        .filter(|pair| {
            let a = pair.first.clone();
            let b = pair.second.clone();

            contains(a.clone(), b.clone()) || contains(b, a)
        })
        .count()
}

fn contains(a: RangeInclusive<u32>, b: RangeInclusive<u32>) -> bool {
    for ia in a {
        if !b.contains(&ia) {
            return false;
        }
    }
    true
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
        let expected = 2;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 503;
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