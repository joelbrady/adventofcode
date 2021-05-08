use std::collections::HashSet;

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

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Clone)]
struct Input {
    rules: Vec<Rule>,
    my_ticket: Ticket,
    nearby_tickets: Vec<Ticket>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Rule {
    label: String,
    ranges: Vec<Range>,
}

impl Rule {
    fn apply(&self, v: i32) -> bool {
        self.ranges.iter().any(|r| v >= r.start && v <= r.end)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct Range {
    start: i32,
    end: i32,
}

#[derive(Debug, Clone)]
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
    input.nearby_tickets.iter()
        .flat_map(|t| t.values.iter().filter(|v| !validate_value_part1(**v, &input.rules)))
        .sum()
}

fn validate_value_part1(value: i32, rules: &[Rule]) -> bool {
    rules.iter()
        .any(|r| r.apply(value))
}

fn solve2(input: &Input) -> i64 {
    let labels = compute_labels(input);

    let departure_fields: HashSet<usize> = labels.iter().enumerate()
        .filter(|(_, label)| label.starts_with("departure"))
        .map(|(i, _)| i)
        .collect();

    input.my_ticket.values.iter()
        .enumerate()
        .filter(|(i, _)| departure_fields.contains(i))
        .map(|(_, n)| *n as i64)
        .product()
}

fn compute_labels(input: &Input) -> Vec<String> {
    let mut p: Vec<HashSet<Rule>> = std::iter::repeat(input.rules.clone())
        .take(input.rules.len())
        .map(|v| v.into_iter().collect())
        .collect();

    let tickets: Vec<Ticket> = std::iter::once(&input.my_ticket)
        .chain(input.nearby_tickets.iter())
        .filter(|t| validate_ticket(t, &input.rules))
        .cloned()
        .collect();

    // remove rules in a position where a value doesn't fit
    for (i, ps) in p.iter_mut().enumerate() {
        for t in tickets.iter() {
            let v = t.values[i];
            let pss = ps.clone();
            for rule in pss {
                if !rule.apply(v) {
                    ps.remove(&rule);
                }
            }
        }
    }

    // remove rules in a position where they are the only possibility in another position
    let mut removed = true;
    while removed {
        removed = false;
        for i in 0..(p.len()) {
            for j in 0..(p.len()) {
                if i != j {
                    let s = p[i].clone();
                    let t = &mut p[j];
                    if s.len() == 1 && t.len() > 1 {
                        s.iter().for_each(|r| {
                            removed = true;
                            t.remove(r);
                        });
                    }
                }
            }
        }
    }

    p.iter()
        .map(|v| {
            debug_assert_eq!(v.len(), 1);
            v.iter().next().unwrap().label.clone()
        })
        .collect()
}

fn validate_ticket(ticket: &Ticket, rules: &[Rule]) -> bool {
    ticket.values.iter()
        .all(|value| validate_value_part1(*value, rules))
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

    #[test]
    fn test_compute_labels() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = vec!["row", "class", "seat"];
        let actual = compute_labels(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 622670335901;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
