use std::collections::{HashMap, HashSet};
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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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

impl From<BinaryNumber> for i64 {
    fn from(n: BinaryNumber) -> Self {
        let s: String = n.into();

        i64::from_str_radix(&s, 2).unwrap()
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
    let ns = &input.ns;
    let length = ns[0].0.len();

    let digits_by_position: HashMap<usize, Vec<BinaryDigit>> = get_digits_by_position(ns);

    let most_common_digit_by_position: HashMap<usize, BinaryDigit> = digits_by_position
        .iter()
        .map(|(k, v)| (*k, *most_common(v)[0]))
        .collect();

    let binary_number: Vec<BinaryDigit> = (0..length)
        .map(|i| most_common_digit_by_position.get(&i).unwrap())
        .copied()
        .collect();

    let binary_number = BinaryNumber(binary_number);
    let epsilon_rate = binary_number.invert();

    let gamma_rate: i64 = binary_number.into();
    let epsilon_rate: i64 = epsilon_rate.into();

    gamma_rate * epsilon_rate
}

fn get_digits_by_position(ns: &[BinaryNumber]) -> HashMap<usize, Vec<BinaryDigit>> {
    let length = ns[0].0.len();

    (0..length)
        .flat_map(|i| ns.iter()
            .map(move |n| (i, n.0[i])))
        .into_group_map()
}

fn most_common<T: Eq + Hash>(ts: &[T]) -> Vec<&T> {
    let counts = ts.iter()
        .counts();

    let max_count = counts.values().max().unwrap();

    counts.iter()
        .filter(|(_, count)| **count == *max_count)
        .map(|(t, _)| *t)
        .collect()
}

fn least_common<T: Eq + Hash>(ts: &[T]) -> Vec<&T> {
    let counts = ts.iter()
        .counts();

    let min_count = counts.values().min().unwrap();

    counts.iter()
        .filter(|(_, count)| **count == *min_count)
        .map(|(t, _)| *t)
        .collect()
}

fn solve2(input: &Input) -> i64 {
    let oxygen = filter_oxygen(&input.ns);
    let oxygen: i64 = oxygen.into();

    let co2 = filter_co2(&input.ns);
    let co2: i64 = co2.into();

    oxygen * co2
}

fn filter_oxygen(ns: &[BinaryNumber]) -> BinaryNumber {
    filter_generic(ns, most_common, &BinaryDigit::One)
}

fn filter_co2(ns: &[BinaryNumber]) -> BinaryNumber {
    filter_generic(ns, least_common, &BinaryDigit::Zero)
}

fn filter_generic<F>(ns: &[BinaryNumber], f: F, default_on_tie: &BinaryDigit) -> BinaryNumber
    where F: Fn(&[BinaryDigit]) -> Vec<&BinaryDigit> {
    let mut candidates: HashSet<BinaryNumber> = ns.iter().cloned().collect();

    assert!(!ns.is_empty());
    while candidates.len() > 1 {
        for i in 0..(ns[0].0.len()) {
            let current: Vec<BinaryNumber> = candidates.iter().cloned().collect();
            let by_position = get_digits_by_position(&current);
            let filter_value = by_position.get(&i).unwrap();
            let filter_value = f(filter_value);
            let filter_value = if filter_value.len() == 1 {
                *filter_value[0]
            } else {
                *default_on_tie
            };
            candidates = candidates.iter()
                .filter(|n| if n.0[i] == filter_value {
                    println!("keeping {:?}", n);
                    true
                } else {
                    false
                })
                .cloned()
                .collect();
        }
    }

    candidates.into_iter().next().unwrap()
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

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 230;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 3414905;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}