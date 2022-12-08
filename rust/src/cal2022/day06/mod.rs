use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

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
    datastream: String,
}

fn parse_input(s: &str) -> Input {
    Input {
        datastream: s.to_owned(),
    }
}

fn solve_part1(input: &Input) -> usize {
    let chars: Vec<char> = input.datastream.chars().collect();

    chars.as_slice()
        .windows(4)
        .enumerate()
        .find(|(_, w)| all_unique(w))
        .map(|(i, _)| i + 4)
        .unwrap()
}

fn all_unique<T: Hash + Eq + Clone + Debug>(cs: &[T]) -> bool {
    cs.iter()
        .cloned()
        .collect::<HashSet<T>>()
        .len() == cs.len()
}

fn solve_part2(input: &Input) -> usize {
    let chars: Vec<char> = input.datastream.chars().collect();

    chars.as_slice()
        .windows(14)
        .enumerate()
        .find(|(_, w)| all_unique(w))
        .map(|(i, _)| i + 14)
        .unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example1() {
        let input = parse_input("bvwbjplbgvbhsrlpgdmjqwftvncz");
        let expected = 5;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1_example2() {
        let input = parse_input("nppdvjthqldpwncqszvftbrmjlhg");
        let expected = 6;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1_example3() {
        let input = parse_input("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg");
        let expected = 10;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1_example4() {
        let input = parse_input("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw");
        let expected = 11;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 1625;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example1() {
        let input = parse_input("mjqjpqmgbljsphdztnvjfqwrcgsmlb");
        let expected = 19;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example2() {
        let input = parse_input("bvwbjplbgvbhsrlpgdmjqwftvncz");
        let expected = 23;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example3() {
        let input = parse_input("nppdvjthqldpwncqszvftbrmjlhg");
        let expected = 23;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example4() {
        let input = parse_input("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg");
        let expected = 29;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example5() {
        let input = parse_input("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw");
        let expected = 26;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 2250;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}