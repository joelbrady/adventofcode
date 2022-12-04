use itertools::Itertools;
use nom::character::complete::line_ending;
use nom::IResult;
use nom::multi::{count, separated_list1};

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
    elves: Vec<Elf>,
}

#[derive(Eq, PartialEq, Debug)]
struct Elf {
    items: Vec<u32>,
}

fn parse_input(s: &str) -> Input {
    let (_, elves) = separated_list1(count(line_ending, 2), parse_elf)(s)
        .unwrap();

    Input { elves }
}

fn parse_elf(input: &str) -> IResult<&str, Elf> {
    let (rem, items) = separated_list1(line_ending, nom::character::complete::u32)(input)?;

    Ok((rem, Elf { items }))
}

fn solve_part1(input: &Input) -> u32 {
    input.elves.iter()
        .map(|e| e.items.iter().sum())
        .max()
        .unwrap()
}

fn solve_part2(input: &Input) -> u32 {
    input.elves.iter()
        .map(|e| e.items.iter().sum::<u32>())
        .sorted()
        .rev()
        .take(3)
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_example() {
        let input = include_str!("example");
        let expected = Input {
            elves: vec![
                Elf {
                    items: vec![1000, 2000, 3000],
                },
                Elf {
                    items: vec![4000],
                },
                Elf {
                    items: vec![5000, 6000],
                },
                Elf {
                    items: vec![7000, 8000, 9000],
                },
                Elf {
                    items: vec![10000],
                },
            ],
        };
        let actual = parse_input(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let actual = solve_part1(&input);
        let expected = 24000;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = include_str!("input");
        let input = parse_input(input);

        let actual = solve_part1(&input);
        let expected = 64929;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let actual = solve_part2(&input);
        let expected = 45000;

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let actual = solve_part2(&input);
        let expected = 193697;

        assert_eq!(actual, expected)
    }
}