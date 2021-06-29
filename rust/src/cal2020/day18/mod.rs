use nom::{IResult, Parser};
use nom::bytes::complete::tag;
use nom::character::complete::{multispace1, space0};
use nom::multi::{many1, separated_list0};
use nom::sequence::preceded;

use crate::parse::parse_i32;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(&input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Eq, PartialEq)]
struct Input {
    expressions: Vec<Vec<Term>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Term {
    Number(i64),
    ParenExp(Vec<Term>),
    Add,
    Multiply,
}

fn parse_input(input: &str) -> Input {
    let (_, expressions) = separated_list0(multispace1, parse_expression)(input).unwrap();

    Input {
        expressions
    }
}

fn parse_expression(input: &str) -> IResult<&str, Vec<Term>> {
    many1(parse_term).parse(input)
}

fn parse_parens(input: &str) -> IResult<&str, Term> {
    tag("(")
        .and(parse_expression.map(Term::ParenExp))
        .and(tag(")"))
        .map(|((_, term), _)| term)
        .parse(input)
}

fn parse_term(input: &str) -> IResult<&str, Term> {
    let p = parse_parens
        .or(parse_number)
        .or(parse_add)
        .or(parse_multiply);

    preceded(space0, p)
        .parse(input)
}

fn parse_number(input: &str) -> IResult<&str, Term> {
    parse_i32
        .map(|n| n as i64)
        .map(Term::Number)
        .parse(input)
}

fn parse_add(input: &str) -> IResult<&str, Term> {
    tag("+")
        .map(|_| Term::Add)
        .parse(input)
}

fn parse_multiply(input: &str) -> IResult<&str, Term> {
    tag("*")
        .map(|_| Term::Multiply)
        .parse(input)
}

fn solve(input: &Input) -> i64 {
    input.expressions.iter()
        .map(|expression| evaluate(expression))
        .sum()
}

fn evaluate(expression: &[Term]) -> i64 {
    let mut stack = Vec::new();

    for term in expression {
        match term {
            Term::ParenExp(e) => stack.push(Term::Number(evaluate(e))),
            _ => stack.push(term.clone())
        }

        if stack.len() == 3 {
            let b = stack.pop().unwrap();
            let op = stack.pop().unwrap();
            let a = stack.pop().unwrap();
            if let Term::Number(a) = a {
                if let Term::Number(b) = b {
                    let op = match op {
                        Term::Add => |(a, b)| a + b,
                        Term::Multiply => |(a, b)| a * b,
                        _ => panic!()
                    };

                    let n = op((a, b));
                    stack.push(Term::Number(n));
                }
            } else {
                panic!("3 terms that aren't an expression")
            }

        }
    }

    if stack.len() != 1 {
        panic!()
    }

    if let Term::Number(n) = &stack[0] {
        *n
    } else {
        panic!()
    }
}

fn solve2(_: &Input) -> i32 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use super::Term::{Add, Multiply, Number, ParenExp};

    #[test]

    fn test_parse_parens() {
        let input = "((1))";

        let expected = Term::ParenExp(vec![Term::ParenExp(vec![Term::Number(1)])]);

        let actual = parse_parens(input).unwrap();

        assert_eq!(actual, ("", expected))
    }

    #[test]
    fn test_parse_number() {
        let input = "123";

        let expected = Number(123);

        let actual = parse_term(input).unwrap();

        assert_eq!(actual, ("", expected));
    }

    #[test]
    fn test_parse_add() {
        let input = "+";

        let expected = Add;

        let actual = parse_term(input).unwrap();

        assert_eq!(actual, ("", expected));
    }

    #[test]
    fn test_parse_number_expression() {
        let input = "123";

        let expected = Input {
            expressions: vec![vec![Number(123)]]
        };

        let actual = parse_input(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_two_expressions() {
        let input = "1 + 2\n3 + 4";

        let expected = Input {
            expressions: vec![
                vec![Number(1), Add, Number(2)],
                vec![Number(3), Add, Number(4)]
            ]
        };

        let actual = parse_input(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_expression() {
        let input = "((1 + (2 * 3) + (4 + 5)))";

        let expected = Input {
            expressions: vec![vec![ParenExp(vec![ParenExp(vec![Number(1), Add, ParenExp(vec![Number(2), Multiply, Number(3)]), Add, ParenExp(vec![Number(4), Add, Number(5)])])])]]
        };

        let actual = parse_input(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_example() {
        use Term::*;

        let input = "1 + 2 * 3 + 4 * 5 + 6";
        let actual = parse_input(input);

        let expected = Input {
            expressions: vec![vec![Number(1), Add, Number(2), Multiply, Number(3), Add, Number(4), Multiply, Number(5), Add, Number(6)]]
        };

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example() {
        let input = "1 + 2 * 3 + 4 * 5 + 6";
        let input = parse_input(input);

        let expected = 71;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example2() {
        let input = "2 * 3 + (4 * 5)";
        let input = parse_input(input);

        let expected = 26;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example3() {
        let input = "5 + (8 * 3 + 9 + 3 * 4 * 3)";
        let input = parse_input(input);

        let expected = 437;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example4() {
        let input = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))";
        let input = parse_input(input);

        let expected = 12240;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example5() {
        let input = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2";
        let input = parse_input(input);

        let expected = 13632;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1408133923393;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 2448;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
