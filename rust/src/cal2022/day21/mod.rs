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
    Number(i128),
    Add(String, String),
    Sub(String, String),
    Mul(String, String),
    Div(String, String),
    Human,
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
    let (rem, n) = nom::character::complete::i128(s)?;

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


fn solve_part1(input: &Input) -> i128 {
    let map: HashMap<String, Expr> = input.monkeys.iter()
        .cloned()
        .map(|m| (m.name, m.expression))
        .collect();

    let root_expr = map.get("root").unwrap();

    if let SExpr::Number(n) = simplify(root_expr, &map) {
        n
    } else {
        panic!()
    }
}

#[derive(Debug, Clone)]
enum SExpr {
    Number(i128),
    Add(Box<SExpr>, Box<SExpr>),
    Sub(Box<SExpr>, Box<SExpr>),
    Mul(Box<SExpr>, Box<SExpr>),
    Div(Box<SExpr>, Box<SExpr>),
    Human,
}

impl SExpr {
    fn assume_num(&self) -> i128 {
        match self {
            SExpr::Number(n) => *n,
            _ => panic!(),
        }
    }
}

fn simplify(expr: &Expr, map: &HashMap<String, Expr>) -> SExpr {
    match expr {
        Expr::Number(n) => SExpr::Number(*n),
        Expr::Add(a, b) => {
            let a = map.get(a).unwrap();
            let a = simplify(a, map);
            let b = map.get(b).unwrap();
            let b = simplify(b, map);

            match a {
                SExpr::Number(a) => {
                    match b {
                        SExpr::Number(b) => SExpr::Number(a + b),
                        e => SExpr::Add(Box::new(SExpr::Number(a)), Box::new(e)),
                    }
                }
                e => SExpr::Add(Box::new(e), Box::new(b))
            }
        }
        Expr::Sub(a, b) => {
            let a = map.get(a).unwrap();
            let a = simplify(a, map);
            let b = map.get(b).unwrap();
            let b = simplify(b, map);

            match a {
                SExpr::Number(a) => {
                    match b {
                        SExpr::Number(b) => SExpr::Number(a - b),
                        e => SExpr::Sub(Box::new(SExpr::Number(a)), Box::new(e)),
                    }
                }
                e => SExpr::Sub(Box::new(e), Box::new(b))
            }
        }
        Expr::Mul(a, b) => {
            let a = map.get(a).unwrap();
            let a = simplify(a, map);
            let b = map.get(b).unwrap();
            let b = simplify(b, map);

            match a {
                SExpr::Number(a) => {
                    match b {
                        SExpr::Number(b) => SExpr::Number(a * b),
                        e => SExpr::Mul(Box::new(SExpr::Number(a)), Box::new(e)),
                    }
                }
                e => SExpr::Mul(Box::new(e), Box::new(b))
            }
        }
        Expr::Div(a, b) => {
            let a = map.get(a).unwrap();
            let a = simplify(a, map);
            let b = map.get(b).unwrap();
            let b = simplify(b, map);

            match a {
                SExpr::Number(a) => {
                    match b {
                        SExpr::Number(b) => SExpr::Number(a / b),
                        e => SExpr::Div(Box::new(SExpr::Number(a)), Box::new(e)),
                    }
                }
                e => SExpr::Div(Box::new(e), Box::new(b))
            }
        }
        Expr::Human => SExpr::Human,
    }
}

fn solve_part2(input: &Input) -> i128 {
    let map = {
        let mut map: HashMap<String, Expr> = input.monkeys.iter()
            .cloned()
            .map(|m| (m.name, m.expression))
            .collect();

        map.insert("humn".to_owned(), Expr::Human);
        map
    };

    let map: HashMap<String, SExpr> = {
        let old = map.clone();
        map.into_iter()
            .map(|(k, v)| (k, simplify(&v, &old)))
            .collect()
    };

    let root = map.get("root").unwrap();
    let (left, right) = match root {
        SExpr::Add(a, b) => (a, b),
        SExpr::Sub(a, b) => (a, b),
        SExpr::Mul(a, b) => (a, b),
        SExpr::Div(a, b) => (a, b),
        _ => panic!("Root did not contain two terms"),
    };

    let (e, target) = if contains_human(left) {
        match right.as_ref() {
            SExpr::Number(n) => (left, n),
            _ => panic!("other side contained human input"),
        }
    } else {
        match left.as_ref() {
            SExpr::Number(n) => (right, n),
            _ => panic!("other side contained human input"),
        }
    };

    let mut expr: SExpr = e.as_ref().clone();
    let mut target = *target;

    loop {
        let next_expr = match &expr {
            SExpr::Number(n) => return *n,
            SExpr::Add(a, b) => {
                if contains_human(a.as_ref()) {
                    target -= b.assume_num();
                    a.as_ref().clone()
                } else {
                    target -= a.assume_num();
                    b.as_ref().clone()
                }
            }
            SExpr::Sub(a, b) => {
                if contains_human(a.as_ref()) {
                    target += b.assume_num();
                    a.as_ref().clone()
                } else {
                    target -= a.assume_num();
                    target *= -1;
                    b.as_ref().clone()
                }
            }
            SExpr::Mul(a, b) => {
                if contains_human(a.as_ref()) {
                    target /= b.assume_num();
                    a.as_ref().clone()
                } else {
                    target /= a.assume_num();
                    b.as_ref().clone()
                }
            }
            SExpr::Div(a, b) => {
                if contains_human(a.as_ref()) {
                    target *= b.assume_num();
                    a.as_ref().clone()
                } else {
                    target *= a.assume_num();
                    b.as_ref().clone()
                }
            }
            SExpr::Human => return target,
        };

        expr = next_expr;
    }
}

fn contains_human(e: &SExpr) -> bool {
    match e {
        SExpr::Number(_) => false,
        SExpr::Add(a, b) => contains_human(a) || contains_human(b),
        SExpr::Sub(a, b) => contains_human(a) || contains_human(b),
        SExpr::Mul(a, b) => contains_human(a) || contains_human(b),
        SExpr::Div(a, b) => contains_human(a) || contains_human(b),
        SExpr::Human => true,
    }
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
        let expected = 301;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 3429411069028;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}