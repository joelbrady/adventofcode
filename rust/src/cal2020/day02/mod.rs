use nom::bytes::complete::{tag, take, take_while};
use nom::character::complete::line_ending;
use nom::character::is_alphabetic;
use nom::IResult;
use nom::multi::separated_list;
use nom::sequence::separated_pair;

use crate::parse::parse_i32;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    // let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    // println!("The solution to part 2 is {}", part2);
}

fn parse_input(input: &str) -> Vec<PasswordCheck> {
    let (_, checks) = separated_list(line_ending, parse_password_check)(input).unwrap();
    checks
}

fn parse_password_check(input: &str) -> IResult<&str, PasswordCheck> {
    let (input, (policy, password)) = separated_pair(
        parse_policy,
        tag(": "),
        take_while(|c| is_alphabetic(c as u8))
    )(input)?;

    Ok((input, PasswordCheck {
        password: password.into(),
        policy,
    }))
}

fn parse_policy(input: &str) -> IResult<&str, Policy> {
    let min_max_parser = separated_pair(parse_i32, tag("-"), parse_i32);
    let (input, ((min, max), char)) = separated_pair(min_max_parser, tag(" "), take(1usize))(input)?;

    Ok((input, Policy {
        character: char.chars().next().unwrap(),
        min: min as usize,
        max: max as usize,
    }))
}

#[derive(Debug)]
struct Policy {
    min: usize,
    max: usize,
    character: char,
}

#[derive(Debug)]
struct PasswordCheck {
    policy: Policy,
    password: String,
}

impl PasswordCheck {
    fn validate(&self) -> bool {
        let occurrences = self.password.chars()
            .filter(|c| *c == self.policy.character)
            .count();

        occurrences >= self.policy.min && occurrences <= self.policy.max
    }
}

fn solve(input: &[PasswordCheck]) -> usize {
    input.iter()
        .filter(|check| check.validate())
        .count()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 2;
        let actual = solve(&input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 620;
        let actual = solve(&input);

        assert_eq!(expected, actual);
    }
}