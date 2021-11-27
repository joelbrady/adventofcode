use nom::{IResult, Parser};
use nom::bytes::complete::tag;
use nom::character::complete::{multispace1, space0};
use nom::multi::{many1, separated_list0};
use nom::sequence::preceded;

use crate::parse::parse_i32;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

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
        println!("stack {:?}", stack);
        panic!()
    }

    if let Term::Number(n) = &stack[0] {
        *n
    } else {
        panic!()
    }
}

fn solve2(input: &Input) -> i64 {
    input.expressions.iter()
        .map(|e| evaluate2(e))
        .sum()
}

fn evaluate2(terms: &[Term]) -> i64 {
    if terms.len() == 1 {
        return match &terms[0] {
            Term::Number(n) => *n,
            Term::ParenExp(inner) => evaluate2(inner),
            _ => panic!(),
        };
    }

    if terms.len() < 3 {
        panic!()
    }

    let terms = evaluate_parens(terms);
    let terms = evaluate_addition(&terms);
    let terms = evaluate_multiply(&terms);

    evaluate2(&terms)
}

fn evaluate_addition(terms: &[Term]) -> Vec<Term> {
    evaluate_operation(terms, &Term::Add, |a, b| a + b)
}

fn evaluate_multiply(terms: &[Term]) -> Vec<Term> {
    evaluate_operation(terms, &Term::Multiply, |a, b| a * b)
}

fn evaluate_operation<F>(terms: &[Term], operation_to_match: &Term, operation: F) -> Vec<Term>
    where F: Fn(i64, i64) -> i64 {
    let mut output = vec![];

    let mut i = 0;
    while i < terms.len() {
        let term = &terms[i];
        match term {
            Term::Number(n) => output.push(Term::Number(*n)),
            Term::ParenExp(expr) => output.push(Term::ParenExp(expr.clone())),
            add_or_multiply => {
                if add_or_multiply == operation_to_match {
                    let a = output.pop().expect("multiply should be preceded by a number");
                    let a: i64 = match a {
                        Term::Number(n) => n,
                        _ => panic!("operator should be preceded by a number")
                    };

                    assert!((i + 1) < terms.len());
                    let b = &terms[i + 1];
                    i += 1;
                    let b: i64 = match b {
                        Term::Number(n) => *n,
                        _ => panic!("operator should be succeeded by a number")
                    };

                    output.push(Term::Number(operation(a, b)))
                } else {
                    output.push(add_or_multiply.clone());
                }
            }
        }
        i += 1;
    }

    output
}

fn evaluate_parens(terms: &[Term]) -> Vec<Term> {
    let mut output = vec![];

    for term in terms {
        let new_term = match term {
            Term::ParenExp(expr) => Term::Number(evaluate2(expr)),
            _ => term.clone(),
        };

        output.push(new_term);
    }

    output
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
                vec![Number(3), Add, Number(4)],
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
    fn test_evaluate_parens_simple_expression() {
        let input = vec![Number(1), Add, ParenExp(vec![Number(23)])];
        let expected = vec![Number(1), Add, Number(23)];

        let actual = evaluate_parens(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_evaluate_parens_nested_expression() {
        let input = vec![Number(1), Add, ParenExp(vec![Number(23), Multiply, Number(10)])];
        let expected = vec![Number(1), Add, Number(230)];

        let actual = evaluate_parens(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_evaluate_multiply() {
        let input = vec![Number(23), Multiply, Number(10)];
        let expected = vec![Number(230)];

        let actual = evaluate_multiply(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_evaluate_multiply2() {
        let input = vec![Number(23), Multiply, Number(10), Multiply, Number(2)];
        let expected = vec![Number(230 * 2)];

        let actual = evaluate_multiply(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example1_part2() {
        let input = "1 + (2 * 3) + (4 * (5 + 6))";
        let input = parse_input(input);

        let expected = 51;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example2_part2() {
        let input = "2 * 3 + (4 * 5)";
        let input = parse_input(input);

        let expected = 46;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example3_part2() {
        let input = "5 + (8 * 3 + 9 + 3 * 4 * 3)";
        let input = parse_input(input);

        let expected = 1445;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example4_part2() {
        let input = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))";
        let input = parse_input(input);

        let expected = 669060;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example5_part2() {
        let input = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2";
        let input = parse_input(input);

        let expected = 23340;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 314455761823725;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
