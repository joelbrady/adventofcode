use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{line_ending, multispace1};
use nom::combinator::map;
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

#[derive(Eq, PartialEq, Debug)]
struct Input {
    rounds: Vec<Round>,
}

#[derive(Eq, PartialEq, Debug)]
struct Round {
    them: Choice,
    us: Choice,
}

#[derive(Eq, PartialEq, Debug)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

#[derive(Debug)]
enum RoundResult {
    Win,
    Lose,
    Draw,
}

impl Choice {
    fn beats(&self, them: &Choice) -> RoundResult {
        use Choice::*;
        use RoundResult::*;

        match self {
            Rock => match them {
                Rock => Draw,
                Paper => Lose,
                Scissors => Win,
            }
            Paper => match them {
                Rock => Win,
                Paper => Draw,
                Scissors => Lose,
            }
            Scissors => match them {
                Rock => Lose,
                Paper => Win,
                Scissors => Draw,
            }
        }
    }
}

fn parse_input(input: &str) -> Input {
    let (_, rounds) = separated_list1(line_ending, parse_round)(input).unwrap();

    Input {
        rounds,
    }
}

fn parse_round(input: &str) -> IResult<&str, Round> {
    let (rem, (them, us)) = separated_pair(parse_choice, multispace1, parse_choice)(input)?;

    Ok((rem, Round { us, them }))
}

fn parse_choice(input: &str) -> IResult<&str, Choice> {
    alt((
        parse_rock,
        parse_paper,
        parse_scissors,
    ))(input)
}

fn parse_rock(input: &str) -> IResult<&str, Choice> {
    let p = alt((
        tag_no_case("A"),
        tag_no_case("X"),
    ));

    map(p, |_| Choice::Rock)(input)
}

fn parse_paper(input: &str) -> IResult<&str, Choice> {
    let p = alt((
        tag_no_case("B"),
        tag_no_case("Y"),
    ));

    map(p, |_| Choice::Paper)(input)
}

fn parse_scissors(input: &str) -> IResult<&str, Choice> {
    let p = alt((
        tag_no_case("C"),
        tag_no_case("Z"),
    ));

    map(p, |_| Choice::Scissors)(input)
}

fn solve_part1(input: &Input) -> u32 {
    input.rounds.iter()
        .map(score_round)
        .sum()
}

fn score_round(round: &Round) -> u32 {
    let result = round.us.beats(&round.them);
    score_choice(&round.us) + score_result(&result)
}

fn score_choice(choice: &Choice) -> u32 {
    match choice {
        Choice::Rock => 1,
        Choice::Paper => 2,
        Choice::Scissors => 3,
    }
}

fn score_result(result: &RoundResult) -> u32 {
    match result {
        RoundResult::Win => 6,
        RoundResult::Lose => 0,
        RoundResult::Draw => 3,
    }
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_score_first_example() {
        let expected = 8;
        let actual = score_round(&Round {
            them: Choice::Rock,
            us: Choice::Paper,
        });

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_score_second_example() {
        let expected = 1;
        let actual = score_round(&Round {
            them: Choice::Paper,
            us: Choice::Rock,
        });

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_score_third_example() {
        let expected = 6;
        let actual = score_round(&Round {
            them: Choice::Scissors,
            us: Choice::Scissors,
        });

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_parse_example() {
        use super::Choice::*;

        let expected = Input {
            rounds: vec![
                Round {
                    them: Rock,
                    us: Paper,
                },
                Round {
                    them: Paper,
                    us: Rock,
                },
                Round {
                    them: Scissors,
                    us: Scissors,
                },
            ],
        };
        let actual = parse_input(include_str!("example"));

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 15;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 14297;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }
}