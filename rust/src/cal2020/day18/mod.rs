use nom::{IResult, Parser};
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::multi::many1;
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
    expression: Vec<Term>,
}

#[derive(Debug, Eq, PartialEq)]
enum Term {
    Number(i32),
    ParenExp(Vec<Term>),
    Add,
    Multiply,
}

fn parse_input(input: &str) -> Input {
    let (_, expression) = parse_expression(input).unwrap();

    Input {
        expression
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

    preceded(multispace0, p)
        .parse(input)
}

fn parse_number(input: &str) -> IResult<&str, Term> {
    parse_i32
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

fn solve(input: &Input) -> i32 {
    todo!()
}

fn solve2(input: &Input) -> i32 {
    todo!()
}

#[cfg(test)]
mod test {
    use crate::cal2020::day18::Term::{Add, Multiply, Number, ParenExp};

    use super::*;

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
            expression: vec![Number(123)]
        };

        let actual = parse_input(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_expression() {
        let input = "((1 + (2 * 3) + (4 + 5)))";

        let expected = Input {
            expression: vec![ParenExp(vec![ParenExp(vec![Number(1), Add, ParenExp(vec![Number(2), Multiply, Number(3)]), Add, ParenExp(vec![Number(4), Add, Number(5)])])])]
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
            expression: vec![Number(1), Add, Number(2), Multiply, Number(3), Add, Number(4), Multiply, Number(5), Add, Number(6)]
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

        let expected = 0;
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
