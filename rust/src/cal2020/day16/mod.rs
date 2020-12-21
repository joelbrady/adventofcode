use nom::bytes::complete::{is_not, tag};
use nom::character::complete::newline;
use nom::IResult;
use nom::multi::separated_list;
use nom::sequence::separated_pair;

use crate::parse::parse_i32;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(&input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);
    //
    // let part2 = solve2(&input);
    // println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    rules: Vec<Rule>,
    my_ticket: Ticket,
    nearby_tickets: Vec<Ticket>,
}

#[derive(Debug)]
struct Rule {
    label: String,
    ranges: Vec<Range>,
}

#[derive(Debug)]
struct Range {
    start: i32,
    end: i32,
}

#[derive(Debug)]
struct Ticket {
    values: Vec<i32>,
}

fn parse_input(input: &str) -> Input {
    let input = input.replace("\r\n", "\n");
    let parts: Vec<&str> = input.split("\n\n").collect();
    let rules = parts[0];
    let my_ticket = parts[1];
    let nearby_tickets = parts[2];
    let (_, rules) = parse_rules(rules).unwrap();
    let my_ticket = parse_my_ticket(my_ticket);
    let nearby_tickets = parse_nearby_tickets(nearby_tickets);

    Input {
        my_ticket,
        nearby_tickets,
        rules,
    }
}

fn parse_rules(input: &str) -> IResult<&str, Vec<Rule>> {
    separated_list(newline, parse_rule)(input)
}

fn parse_rule(input: &str) -> IResult<&str, Rule> {
    let (input, (label, ranges)) = separated_pair(
        parse_label,
        tag(": "),
        parse_ranges,
    )(input)?;

    Ok((input, Rule {
        label: label.into(),
        ranges,
    }))
}

fn parse_label(input: &str) -> IResult<&str, &str> {
    is_not(":")(input)
}

fn parse_ranges(input: &str) -> IResult<&str, Vec<Range>> {
    separated_list(tag(" or "), parse_range)(input)
}

fn parse_range(input: &str) -> IResult<&str, Range> {
    let (input, (start, end)) = separated_pair(parse_i32, tag("-"), parse_i32)(input)?;

    Ok((input, Range {
        start,
        end,
    }))
}

fn parse_my_ticket(input: &str) -> Ticket {
    let values = input.lines()
        .skip(1)
        .map(|s| parse_values(s))
        .next()
        .unwrap();

    Ticket {
        values,
    }
}

fn parse_values(input: &str) -> Vec<i32> {
    input.split(',')
        .map(|s| s.parse().unwrap())
        .collect()
}

fn parse_nearby_tickets(input: &str) -> Vec<Ticket> {
    input.lines()
        .skip(1)
        .map(|s| parse_values(s))
        .map(|values| Ticket { values })
        .collect()
}

fn solve(input: &Input) -> i32 {
    let invalid_tickets: Vec<&Ticket> = input.nearby_tickets.iter()
        .filter(|t| t.values.iter().any(|v| !validate_value_part1(*v, &input.rules)))
        .collect();

    let invalid_values: Vec<i32> = invalid_tickets.iter()
        .flat_map(|t| t.values.iter()
            .filter(|v| !validate_value_part1(**v, &input.rules)))
        .copied()
        .collect();

    invalid_values.iter()
        .sum()
}

fn validate_value_part1(value: i32, rules: &[Rule]) -> bool {
    rules.iter()
        .flat_map(|r| r.ranges.iter())
        .any(|range| value >= range.start && value <= range.end)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 71;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 25972;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }
}