use std::collections::HashMap;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

struct Input {
    rules: Vec<Rule>,
    messages: Vec<String>,
}

#[derive(Debug, Eq, PartialEq)]
struct Rule {
    label: Label,
    pattern: Pattern,
}

impl Rule {}

#[derive(Debug, Eq, PartialEq)]
enum MatchResult<'a> {
    Match(&'a str),
    NoMatch(&'a str),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
struct Label(String);

impl From<&str> for Label {
    fn from(s: &str) -> Self {
        Label(s.to_owned())
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Pattern {
    Literal(String),
    Alternative(Box<Alternative>),
    Sequence(Vec<Label>),
}

impl Pattern {
    fn new_literal(s: &str) -> Pattern {
        Pattern::Literal(s.to_owned())
    }

    fn consume<'a>(&self, dictionary: &HashMap<Label, &Rule>, message: &'a str) -> MatchResult<'a> {
        match &self {
            Pattern::Literal(s) => {
                assert_eq!(s.len(), 1);
                if message.starts_with(s) {
                    MatchResult::Match(&message[1..])
                } else {
                    MatchResult::NoMatch(message)
                }
            }
            Pattern::Alternative(alt) => {
                let left = &alt.left;
                let right = &alt.right;

                match left.consume(dictionary, message) {
                    MatchResult::Match(remainder) => MatchResult::Match(remainder),
                    MatchResult::NoMatch(remainder) => right.consume(dictionary, remainder),
                }
            }
            Pattern::Sequence(labels) => {
                let result = labels.iter()
                    .fold(MatchResult::Match(message), |acc, label| {
                        if let MatchResult::Match(remainder) = acc {
                            let rule = dictionary.get(label).expect("all labels should be  valid");

                            rule.pattern.consume(dictionary, remainder)
                        } else {
                            acc
                        }
                    });

                match result {
                    MatchResult::Match(remainder) => MatchResult::Match(remainder),
                    MatchResult::NoMatch(_) => MatchResult::NoMatch(message),
                }
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Alternative {
    left: Pattern,
    right: Pattern,
}

fn parse_input(input: &str) -> Input {
    let rules: Vec<&str> = input.lines()
        .collect();

    let a: Vec<&[&str]> = rules.split(|line| line.trim().is_empty()).collect();

    assert_eq!(a.len(), 2);

    let rules = a[0].iter()
        .map(|line| parse_rule(line))
        .collect();

    let messages = a[1].iter()
        .map(|s| String::from(*s))
        .collect();

    Input {
        rules,
        messages,
    }
}

fn parse_rule(line: &str) -> Rule {
    let split: Vec<&str> = line.split(": ").collect();
    assert_eq!(split.len(), 2);

    let label = split[0];
    let label = label.into();
    let pattern = parse_pattern(split[1]);

    Rule {
        label,
        pattern,
    }
}

fn parse_pattern(s: &str) -> Pattern {
    if s.starts_with('\"') {
        Pattern::new_literal(&s[1..2])
    } else {
        let split: Vec<&str> = s.split(" | ").collect();

        if split.len() > 1 {
            assert_eq!(split.len(), 2);
            let left = parse_sequence(split[0]);
            let right = parse_sequence(split[1]);
            Pattern::Alternative(Box::new(Alternative { left, right }))
        } else {
            parse_sequence(split[0])
        }
    }
}

fn parse_sequence(s: &str) -> Pattern {
    let labels = s.split(' ')
        .map(|s| s.into())
        .collect();

    Pattern::Sequence(labels)
}

fn solve(input: &Input) -> usize {
    let dictionary: HashMap<Label, &Rule> = input.rules.iter()
        .map(|rule| (rule.label.clone(), rule))
        .collect();

    let rule = dictionary.get(&"0".into()).expect("there should always be a rule 0");

    input.messages.iter()
        .filter(|message|
            match rule.pattern.consume(&dictionary, message) {
                MatchResult::Match(remainder) => {
                    remainder.is_empty()
                }
                MatchResult::NoMatch(_) => {
                    false
                }
            }
        )
        .count()
}

fn solve2(_: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_line() {
        let input = r#"4: "a""#;
        let expected = Rule {
            label: "4".into(),
            pattern: Pattern::new_literal("a"),
        };

        let actual = parse_rule(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_parse_line_sequence() {
        let input = r#"2: 4 1 2"#;
        let expected = Rule {
            label: "2".into(),
            pattern: Pattern::Sequence(vec!["4".into(), "1".into(), "2".into()]),
        };

        let actual = parse_rule(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_parse_line_alternative() {
        let input = r#"123: 7 8 | 9 10"#;
        let alternative = Alternative {
            left: Pattern::Sequence(vec!["7".into(), "8".into()]),
            right: Pattern::Sequence(vec!["9".into(), "10".into()]),
        };
        let expected = Rule {
            label: "123".into(),
            pattern: Pattern::Alternative(Box::new(alternative)),
        };

        let actual = parse_rule(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_pattern_consume_literal() {
        let dictionary: HashMap<Label, &Rule> = HashMap::new();
        let pattern = Pattern::new_literal("a");
        let expected = MatchResult::Match("");

        let actual = pattern.consume(&dictionary, "a");

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_pattern_consume_literal_with_remainder() {
        let dictionary: HashMap<Label, &Rule> = HashMap::new();
        let pattern = Pattern::new_literal("a");
        let expected = MatchResult::Match("b");

        let actual = pattern.consume(&dictionary, "ab");

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_pattern_consume_sequence_one_element() {
        let rule_one = Rule {
            pattern: Pattern::new_literal("b"),
            label: Label::from("1"),
        };
        let dictionary: HashMap<Label, &Rule> = vec![&rule_one]
            .iter()
            .map(|r| (r.label.clone(), *r))
            .collect();
        let pattern = Pattern::Sequence(vec![Label::from("1")]);
        let expected = MatchResult::Match("");

        let actual = pattern.consume(&dictionary, "b");

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_pattern_consume_sequence_two_elements() {
        let rule_one = Rule {
            pattern: Pattern::new_literal("b"),
            label: Label::from("1"),
        };
        let rule_two = Rule {
            pattern: Pattern::new_literal("c"),
            label: Label::from("2"),
        };
        let dictionary: HashMap<Label, &Rule> = vec![&rule_one, &rule_two]
            .iter()
            .map(|r| (r.label.clone(), *r))
            .collect();
        let pattern = Pattern::Sequence(vec![rule_one.label.clone(), rule_two.label.clone()]);
        let expected = MatchResult::Match("");

        let actual = pattern.consume(&dictionary, "bc");

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_pattern_consume_sequence_alternative() {
        let rule_one = Rule {
            pattern: Pattern::new_literal("b"),
            label: Label::from("1"),
        };
        let rule_two = Rule {
            pattern: Pattern::new_literal("c"),
            label: Label::from("2"),
        };
        let dictionary: HashMap<Label, &Rule> = vec![&rule_one, &rule_two]
            .iter()
            .map(|r| (r.label.clone(), *r))
            .collect();
        let left = Pattern::Sequence(vec![rule_one.label.clone(), rule_two.label.clone()]);
        let right = Pattern::Sequence(vec![rule_two.label.clone(), rule_one.label.clone()]);
        let alt = Alternative {
            left,
            right
        };
        let pattern = Pattern::Alternative(Box::new(alt));
        let expected = MatchResult::Match("");

        let actual = pattern.consume(&dictionary, "bc");

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 2;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 132;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }
}