use std::collections::HashSet;

use nom::bytes::complete::is_not;
use nom::character::complete::{line_ending, multispace1};
use nom::IResult;
use nom::multi::{many1, separated_list};

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

fn solve(input: &[Group]) -> i32 {
    input.iter()
        .map(|group| total_yeses(group))
        .sum()
}

fn solve2(input: &[Group]) -> i32 {
    input.iter()
        .map(|group| all_yeses(group))
        .sum()
}

fn total_yeses(group: &Group) -> i32 {
    let yeses_for_group: HashSet<char> = group.forms.iter()
        .flat_map(|set| set.yeses.iter().copied())
        .collect();

    yeses_for_group.len() as i32
}

fn all_yeses(group: &Group) -> i32 {
    let mut yeses = group.forms[0].yeses.clone();
    for form in group.forms.iter() {
        yeses = yeses.intersection(&form.yeses).copied().collect();
    }

    yeses.len() as i32
}

#[derive(Debug, Eq, PartialEq)]
struct Group {
    forms: Vec<Form>,
}

#[derive(Debug, Eq, PartialEq)]
struct Form {
    yeses: HashSet<char>,
}

fn parse_input(input: &str) -> Vec<Group> {
    let (_, groups) = separated_list(multispace1, parse_group)(input).unwrap();

    groups
}

fn parse_group(input: &str) -> IResult<&str, Group> {
    let (input, forms) = separated_list(line_ending, parse_form)(input)?;

    Ok((input, Group { forms }))
}

fn parse_form(input: &str) -> IResult<&str, Form> {
    let (input, yeses) = many1(is_not("\r\n"))(input)?;

    let yeses = yeses.iter().flat_map(|s| s.chars()).collect();

    Ok((input, Form { yeses }))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_group() {
        let input = "a\r\nbc";

        let actual = parse_group(input);

        let form1 = Form { yeses: ['a'].iter().copied().collect() };
        let form2 = Form { yeses: ['b', 'c'].iter().copied().collect() };

        assert_eq!(actual, Ok(("", Group { forms: vec![form1, form2] })))
    }

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        assert_eq!(input.len(), 5);

        let expected = 11;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 7120;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 3570;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}