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
        .filter_map(|l| find_first_invalid_closing_char(*l))
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

fn find_first_invalid_closing_char(s: &str) -> Option<char> {
    let mut stack = vec![];

    for c in s.chars() {
        match c {
            '(' | '[' | '{' | '<' => stack.push(c),
            ')' | ']' | '}' | '>' =>
                if let Some(p) = stack.pop() {
                    if p == matching(c) {
                        continue;
                    } else {
                        return Some(c);
                    }
                } else {
                    return Some(c);
                }
            _ => panic!("unexpected {:?}", c),
        }
    }

    None
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

fn solve2(_: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_first_invalid_closing_char() {
        let input = "()";
        let expected = None;

        let actual = find_first_invalid_closing_char(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char2() {
        let input = "(())";
        let expected = None;

        let actual = find_first_invalid_closing_char(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char3() {
        let input = "(())()()((()()))";
        let expected = None;

        let actual = find_first_invalid_closing_char(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char4() {
        let input = "(())()()((()())";
        let expected = None;

        let actual = find_first_invalid_closing_char(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_find_first_invalid_closing_char5() {
        let input = "(())()()((()())))";
        let expected = Some(')');

        let actual = find_first_invalid_closing_char(input);

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
}