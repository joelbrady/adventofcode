use nom::bytes::complete::{tag, take, take_while};
use nom::character::complete::line_ending;
use nom::character::is_alphabetic;
use nom::IResult;
use nom::multi::separated_list1;
use nom::sequence::separated_pair;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

fn parse_input(input: &str) -> Vec<PasswordCheck> {
    let (_, checks) = separated_list1(line_ending, parse_password_check)(input).unwrap();
    checks
}

fn parse_password_check(input: &str) -> IResult<&str, PasswordCheck> {
    let (input, (policy, password)) = separated_pair(
        parse_policy,
        tag(": "),
        take_while(|c| is_alphabetic(c as u8)),
    )(input)?;

    Ok((input, PasswordCheck {
        password: password.into(),
        policy,
    }))
}

fn parse_policy(input: &str) -> IResult<&str, Policy> {
    let parse_i32 = nom::character::complete::i32;

    let min_max_parser = separated_pair(parse_i32, tag("-"), parse_i32);
    let (input, ((min, max), char)) = separated_pair(min_max_parser, tag(" "), take(1usize))(input)?;

    Ok((input, Policy {
        character: char,
        min: min as usize,
        max: max as usize,
    }))
}

#[derive(Debug)]
struct Policy<'a> {
    min: usize,
    max: usize,
    character: &'a str,
}

#[derive(Debug)]
struct PasswordCheck<'a> {
    policy: Policy<'a>,
    password: String,
}

impl PasswordCheck<'_> {
    fn validate(&self) -> bool {
        let occurrences = self.password.matches(self.policy.character)
            .count();

        occurrences >= self.policy.min && occurrences <= self.policy.max
    }

    fn validate2(&self) -> bool {
        let pos_1 = self.policy.min - 1;
        let pos_2 = self.policy.max - 1;

        let char_1 = self.password.get(pos_1..=pos_1);
        let char_2 = self.password.get(pos_2..=pos_2);

        vec![char_1, char_2]
            .iter()
            .filter_map(|a| *a)
            .filter(|c| *c == self.policy.character)
            .count() == 1
    }
}

fn solve(input: &[PasswordCheck<'static>]) -> usize {
    input.iter()
        .filter(|check| check.validate())
        .count()
}

fn solve2(input: &[PasswordCheck<'static>]) -> usize {
    input.iter()
        .filter(|check| check.validate2())
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

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 1;
        let actual = solve2(&input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 727;
        let actual = solve2(&input);

        assert_eq!(expected, actual);
    }
}