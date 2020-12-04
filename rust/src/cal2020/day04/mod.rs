use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while};
use nom::character::complete::{line_ending, multispace1, one_of};
use nom::IResult;
use nom::multi::separated_list;
use nom::sequence::separated_pair;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    // let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    // println!("The solution to part 2 is {}", part2);
}

fn solve(input: &[HashMap<&str, &str>]) -> usize {
    input.iter()
        .filter(|p| is_passport_valid(*p))
        .count()
}

fn is_passport_valid(passport: &HashMap<&str, &str>) -> bool {
    passport.keys().len() == 8 || (passport.keys().len() == 7 && !passport.contains_key("cid"))
}

// fn solve2(input: &[i32]) -> i32 {
//     unimplemented!()
// }

fn parse_input(input: &str) -> Vec<HashMap<&str, &str>> {
    let (_, passports) = separated_list(multispace1, parse_passport)(input).unwrap();

    passports
}

fn parse_passport(input: &str) -> IResult<&str, HashMap<&str, &str>> {
    let (input, pairs) = separated_list(alt((tag(" "), line_ending)), parse_pair)(input)?;

    Ok((input, pairs.iter().copied().collect()))
}

fn parse_pair(input: &str) -> IResult<&str, (&str, &str)> {
    separated_pair(key_val_format, one_of(":"), key_val_format)(input)
}

fn key_val_format(input: &str) -> IResult<&str, &str> {
    is_not(" :\r\n\t")(input)
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn test_parse_pair() {
        let actual = parse_pair("abc:123");

        assert_eq!(actual, Ok(("", ("abc", "123"))))
    }

    #[test]
    fn test_parse_passport() {
        let actual = parse_passport("a:1 b:2\nc:3");

        let map = [
            ("a", "1"),
            ("b", "2"),
            ("c", "3")
        ].iter().copied().collect();

        assert_eq!(actual, Ok(("", map)))
    }

    #[test]
    fn test_not_space() {
        let actual = key_val_format("ab c");

        assert_eq!(actual, Ok((" c", "ab")))
    }

    #[test]
    fn test_parse_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = [
            ("ecl", "gry"),
            ("pid", "860033327"),
            ("eyr", "2020"),
            ("hcl", "#fffffd"),
            ("byr", "1937"),
            ("iyr", "2017"),
            ("cid", "147"),
            ("hgt", "183cm")
        ].iter().cloned().collect();

        let actual = input[0].clone();

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 2;
        let actual = solve(&input);

        assert_eq!(expected, actual)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 239;
        let actual = solve(&input);

        assert_eq!(expected, actual)
    }
}