use itertools::Itertools;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input<'a> {
    lines: Vec<&'a str>,
}

fn parse_input(s: &str) -> Input {
    let lines = s.lines().collect();

    Input { lines }
}

fn solve(input: &Input) -> i64 {
    input.lines.iter()
        .filter_map(|l| match parse(*l) {
            ParseResult::Invalid(c) => Some(c),
            ParseResult::Incomplete(_) => None,
            ParseResult::Valid => None,
        })
        .map(score_char)
        .sum()
}

fn matching(c: char) -> char {
    match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        ')' => '(',
        ']' => '[',
        '}' => '{',
        '>' => '<',
        _ => panic!("unexpected {:?}", c)
    }
}

#[derive(Debug, Eq, PartialEq)]
enum ParseResult {
    Invalid(char),
    Incomplete(Vec<char>),
    Valid,
}

fn parse(s: &str) -> ParseResult {
    let mut stack = vec![];

    for c in s.chars() {
        match c {
            '(' | '[' | '{' | '<' => stack.push(c),
            ')' | ']' | '}' | '>' =>
                if let Some(p) = stack.pop() {
                    if p == matching(c) {
                        continue;
                    } else {
                        return ParseResult::Invalid(c);
                    }
                } else {
                    return ParseResult::Invalid(c);
                }
            _ => panic!("unexpected {:?}", c),
        }
    }

    if stack.is_empty() {
        ParseResult::Valid
    } else {
        ParseResult::Incomplete(stack)
    }
}

fn score_char(c: char) -> i64 {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => panic!("unexpected {:?}", c)
    }
}

fn solve2(input: &Input) -> u64 {
    let scores: Vec<u64> = input.lines.iter()
        .filter_map(|l| match parse(*l) {
            ParseResult::Invalid(_) => None,
            ParseResult::Incomplete(v) => Some(v),
            ParseResult::Valid => None,
        })
        .map(|v| score_autocomplete(&v))
        .sorted()
        .collect();

    let median_index = scores.len() / 2;

    scores[median_index]
}

fn score_autocomplete(cs: &[char]) -> u64 {
    let chars_to_complete: Vec<char> = cs.iter()
        .rev()
        .map(|c| matching(*c))
        .collect();

    let mut score = 0;

    for c in chars_to_complete {
        score *= 5;
        score += match c {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _ => panic!("unexpected {:?}", c)
        }
    }

    score
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_first_invalid_closing_char() {
        let input = "()";
        let expected = ParseResult::Valid;

        let actual = parse(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char2() {
        let input = "(())";
        let expected = ParseResult::Valid;

        let actual = parse(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char3() {
        let input = "(())()()((()()))";
        let expected = ParseResult::Valid;

        let actual = parse(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char4() {
        let input = "(())()()((()())";
        let expected = ParseResult::Incomplete(vec!['(']);

        let actual = parse(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char5() {
        let input = "(())()()((()())))";
        let expected = ParseResult::Invalid(')');

        let actual = parse(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 26397;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 370407;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 288957;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 3249889609;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}