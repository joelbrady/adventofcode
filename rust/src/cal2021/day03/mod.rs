use std::collections::HashMap;
use std::hash::Hash;

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
struct Input {
    ns: Vec<BinaryNumber>,
}

#[derive(Debug, Clone)]
struct BinaryNumber(Vec<BinaryDigit>);

impl BinaryNumber {
    fn invert(&self) -> BinaryNumber {
        BinaryNumber(self.0.iter()
            .map(|d| match d {
                BinaryDigit::Zero => BinaryDigit::One,
                BinaryDigit::One => BinaryDigit::Zero,
            })
            .collect()
        )
    }
}

impl From<BinaryNumber> for String {
    fn from(n: BinaryNumber) -> Self {
        n.0.iter()
            .map(|d| match d {
                BinaryDigit::Zero => '0',
                BinaryDigit::One => '1',
            })
            .collect()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum BinaryDigit {
    Zero,
    One,
}

fn parse_input(s: &str) -> Input {
    let ns = s.lines()
        .map(parse_binary_number)
        .collect();

    Input {
        ns
    }
}

fn parse_binary_number(s: &str) -> BinaryNumber {
    let f = s.chars()
        .map(|c| match c {
            '1' => BinaryDigit::One,
            '0' => BinaryDigit::Zero,
            _ => panic!(),
        })
        .collect();

    BinaryNumber(f)
}

fn solve(input: &Input) -> i64 {
    let length = input.ns[0].0.len();

    let digits_by_position = (0..length)
        .flat_map(|i| input.ns.iter()
            .map(move |n| (i, n.0[i])))
        .into_group_map();

    let most_common_digit_by_position: HashMap<usize, BinaryDigit> = digits_by_position
        .iter()
        .map(|(k, v)| (*k, most_common(v)))
        .collect();

    let binary_number: Vec<BinaryDigit> = (0..length)
        .map(|i| most_common_digit_by_position.get(&i).unwrap())
        .copied()
        .collect();

    let binary_number = BinaryNumber(binary_number);

    let gamma_rate: String = binary_number.clone().into();

    let gamma_rate = i64::from_str_radix(&gamma_rate, 2).unwrap();

    let epsilon_rate = binary_number.invert();
    let epsilon_rate: String = epsilon_rate.into();
    let epsilon_rate = i64::from_str_radix(&epsilon_rate, 2).unwrap();

    gamma_rate * epsilon_rate
}

fn most_common<T: Eq + Hash + Copy>(ts: &[T]) -> T {
    ts.iter()
        .counts()
        .iter()
        .max_by_key(|(_, count)| *count)
        .map(|(t, _)| **t)
        .unwrap()
}

fn solve2(_: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 198;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 4191876;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }
}