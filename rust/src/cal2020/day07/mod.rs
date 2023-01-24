use std::collections::HashMap;
use std::collections::HashSet;

use nom::{FindSubstring, IResult, Slice};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{separated_pair, terminated, tuple};

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(input.clone().leak(), "shiny gold");
    let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

fn solve2(input: &[Rule]) -> u32 {
    dfs2(input, "shiny gold")
}

fn dfs2(input: &[Rule], target: &str) -> u32 {
    let mut total = 0;
    for rule in input.iter() {
        if rule.color == target {
            if rule.contents.is_empty() {
                total = 0;
            } else {
                for (color, count) in rule.contents.iter() {
                    total += count;
                    total += count * dfs2(input, color);
                }
            }
        }
    }

    total
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Rule<'a> {
    color: &'a str,
    contents: HashMap<&'a str, u32>,
}

fn parse_input(input: &str) -> Vec<Rule> {
    let (_, rules) = separated_list1(line_ending, parse_rule)(input).unwrap();

    rules
}

fn parse_rule(input: &str) -> IResult<&str, Rule> {
    // light red bags contain 1 bright white bag, 2 muted yellow bags.
    let (input, (color, contents)) = terminated(separated_pair(parse_color, tag(" contain "), parse_contents), tag("."))(input)?;

    Ok((input, Rule { color, contents }))
}

fn parse_color(input: &str) -> IResult<&str, &str> {
    // should get color from strings starting with:
    // * light red bags
    // * bright white bag

    let bags_location = input.find_substring("bags");
    let bag_location = input.find_substring("bag");

    match bags_location {
        None => match bag_location {
            None => unimplemented!(),
            Some(_) => get_phrase_ending_in(" bag")(input),
        },
        Some(bags) => match bag_location {
            None => get_phrase_ending_in(" bags")(input),
            Some(bag) => {
                if bag < bags {
                    get_phrase_ending_in(" bag")(input)
                } else {
                    get_phrase_ending_in(" bags")(input)
                }
            }
        },
    }
}

fn get_phrase_ending_in(delimiter: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |input| {
        let color = input.split(delimiter)
            .next()
            .unwrap();

        let remainder = input.slice((color.len() + delimiter.len())..);
        Ok((remainder, color))
    }
}

fn parse_contents(input: &str) -> IResult<&str, HashMap<&str, u32>> {
    let empty_case = map(tag("no other bags"), |_| HashMap::new());
    let otherwise = map(
        separated_list1(tag(", "), parse_content_pair),
        |pairs| pairs.into_iter().collect(),
    );

    alt((empty_case, otherwise))(input)
}

fn parse_content_pair(input: &str) -> IResult<&str, (&str, u32)> {
    let parse_i32 = nom::character::complete::i32;

    let (input, (amount, _, color)) = tuple((parse_i32, tag(" "), parse_color))(input)?;

    Ok((input, (color, amount as u32)))
}

fn solve(input: &'static [Rule], target: &str) -> usize {
    let mut candidates = HashSet::new();

    dfs(input, &mut candidates, target);

    candidates.len()
}

fn dfs(input: &'static [Rule], candidates: &mut HashSet<&str>, target: &str) {
    for rule in input.iter() {
        if rule.contents.contains_key(target) {
            candidates.insert(rule.color);
            dfs(input, candidates, rule.color);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_color() {
        let input = "light red bags contain 1 bright white bag, 2 muted yellow bags.";
        let expected = Ok((" contain 1 bright white bag, 2 muted yellow bags.", "light red"));

        let actual = parse_color(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_parse_color2() {
        let input = "bright white bag, 2 muted yellow bags.";
        let expected = Ok((", 2 muted yellow bags.", "bright white"));

        let actual = parse_color(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_parse_rule() {
        let input = "light red bags contain 1 bright white bag, 2 muted yellow bags.";
        let expected = Ok(("", Rule {
            color: "light red",
            contents: [("bright white", 1), ("muted yellow", 2)].iter().copied().collect(),
        }));

        let actual = parse_rule(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_parse_rule2() {
        let input = "dotted black bags contain no other bags.\r\nlight red bags contain 1 bright white bag, 2 muted yellow bags.";
        let expected = Ok(("\r\nlight red bags contain 1 bright white bag, 2 muted yellow bags.", Rule {
            color: "dotted black",
            contents: HashMap::new(),
        }));

        let actual = parse_rule(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 4;
        let actual = solve(input.clone().leak(), "shiny gold");

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 235;
        let actual = solve(input.clone().leak(), "shiny gold");

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example1_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 32;

        let actual = solve2(input.clone().leak());

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example2_part2() {
        let input = include_str!("example_part2");
        let input = parse_input(input);

        let expected = 126;

        let actual = solve2(input.clone().leak());

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve2_1_recurse() {
        let input = vec![
            Rule { color: "shiny gold", contents: [("blue", 2)].iter().copied().collect() },
            Rule { color: "blue", contents: HashMap::new() },
        ];

        let actual = solve2(&input);

        assert_eq!(actual, 2)
    }


    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 158493;
        let actual = solve2(input.clone().leak());

        assert_eq!(actual, expected)
    }
}