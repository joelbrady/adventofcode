use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{alpha1, line_ending};
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
    monkeys: Vec<Monkey>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Monkey {
    name: String,
    expression: Expr,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Expr {
    Number(i64),
    Add(String, String),
    Sub(String, String),
    Mul(String, String),
    Div(String, String),
}

fn parse_input(s: &str) -> Input {
    let (_, monkeys) = separated_list1(line_ending, parse_monkey)(s).unwrap();

    Input { monkeys }
}

fn parse_monkey(s: &str) -> IResult<&str, Monkey> {
    let (rem, (name, expression)) = separated_pair(
        is_not(":"),
        tag(": "),
        parse_expression,
    )(s)?;

    Ok((rem, Monkey { name: name.to_owned(), expression }))
}

fn parse_expression(s: &str) -> IResult<&str, Expr> {
    alt((
        parse_number,
        parse_add,
        parse_sub,
        parse_mul,
        parse_div,
    ))(s)
}

fn parse_number(s: &str) -> IResult<&str, Expr> {
    let (rem, n) = nom::character::complete::i64(s)?;

    Ok((rem, Expr::Number(n)))
}

fn parse_add(s: &str) -> IResult<&str, Expr> {
    let (rem, (a, b)) = separated_pair(alpha1, tag(" + "), alpha1)(s)?;

    Ok((rem, Expr::Add(a.to_owned(), b.to_owned())))
}

fn parse_sub(s: &str) -> IResult<&str, Expr> {
    let (rem, (a, b)) = separated_pair(alpha1, tag(" - "), alpha1)(s)?;

    Ok((rem, Expr::Sub(a.to_owned(), b.to_owned())))
}

fn parse_mul(s: &str) -> IResult<&str, Expr> {
    let (rem, (a, b)) = separated_pair(alpha1, tag(" * "), alpha1)(s)?;

    Ok((rem, Expr::Mul(a.to_owned(), b.to_owned())))
}
fn parse_div(s: &str) -> IResult<&str, Expr> {
    let (rem, (a, b)) = separated_pair(alpha1, tag(" / "), alpha1)(s)?;

    Ok((rem, Expr::Div(a.to_owned(), b.to_owned())))
}


fn solve_part1(input: &Input) -> i64 {
    let map: HashMap<String, Expr> = input.monkeys.iter()
        .cloned()
        .map(|m| (m.name, m.expression))
        .collect();

    let root_expr = map.get("root").unwrap();

    eval(root_expr, &map)
}

fn eval(expr: &Expr, map: &HashMap<String, Expr>) -> i64 {
    match expr {
        Expr::Number(n) => *n,
        Expr::Add(a, b) => {
            let a = map.get(a).unwrap();
            let b = map.get(b).unwrap();

            eval(a, map) + eval(b, map)
        }
        Expr::Sub(a, b) => {
            let a = map.get(a).unwrap();
            let b = map.get(b).unwrap();

            eval(a, map) - eval(b, map)
        }
        Expr::Mul(a, b) => {
            let a = map.get(a).unwrap();
            let b = map.get(b).unwrap();

            eval(a, map) * eval(b, map)
        }
        Expr::Div(a, b) => {
            let a = map.get(a).unwrap();
            let b = map.get(b).unwrap();

            eval(a, map) / eval(b, map)
        }
    }
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_number() {
        let expected = Expr::Number(42);
        let actual = parse_number("42").unwrap();

        assert_eq!(actual, ("", expected))
    }

    #[test]
    fn test_parse_add() {
        let expected = Expr::Add("zf".to_owned(), "hl".to_owned());
        let actual = parse_add("zf + hl").unwrap();

        assert_eq!(actual, ("", expected))
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 152;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 82225382988628;
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