use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while};
use nom::character::complete::{line_ending, multispace1, one_of};
use nom::character::is_hex_digit;
use nom::IResult;
use nom::multi::{count, separated_list1};
use nom::sequence::{preceded, separated_pair};

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

fn solve(input: &[HashMap<&str, &str>]) -> usize {
    input.iter()
        .filter(|p| is_passport_valid(*p))
        .count()
}

fn is_passport_valid(passport: &HashMap<&str, &str>) -> bool {
    passport.keys().len() == 8 || (passport.keys().len() == 7 && !passport.contains_key("cid"))
}

fn is_passport_valid2(passport: &HashMap<&str, &str>) -> Result<(), &'static str> {
    if !(is_passport_valid(passport)) {
        Err("stage 1")
    } else {
        validate_birth_year(passport["byr"])?;
        validate_issue_year(passport["iyr"])?;
        validate_expiration_year(passport["eyr"])?;
        validate_height(passport["hgt"])?;
        validate_hair_color(passport["hcl"])?;
        validate_eye_color(passport["ecl"])?;
        validate_passport_id(passport["pid"])?;
        // cid (Country ID) - ignored, missing or not.

        Ok(())
    }
}

fn validate_passport_id(input: &str) -> Result<(), &'static str> {
    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    if input.len() == 9 && input.chars().all(|c| c >= '0' && c <= '9') {
        Ok(())
    } else {
        Err("invalid passport id")
    }
}

fn validate_eye_color(input: &str) -> Result<(), &'static str> {
    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    if ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        .iter()
        .any(|color| input == *color) {
        Ok(())
    } else {
        Err("unknown eye color")
    }
}

fn validate_hair_color(input: &str) -> Result<(), &'static str> {
    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    let result: IResult<&str, _> = preceded(tag("#"), count(take_while(|c| is_hex_digit(c as u8)), 6))(input);

    match result {
        Ok(_) => Ok(()),
        Err(_) => Err("hair color validation failed")
    }
}

fn validate_height(input: &str) -> Result<(), &'static str> {
    // hgt (Height) - a number followed by either cm or in:
    if input.contains("cm") {
        let mut a = input.split("cm");
        match a.next() {
            Some(number) => {
                let n: u32 = number.parse().map_err(|_| "height bad 2")?;
                // If cm, the number must be at least 150 and at most 193.
                if n >= 150 && n <= 193 {
                    Ok(())
                } else {
                    Err("height cm range incorrect")
                }
            },
            None => Err("height bad 1")
        }
    } else if input.contains("in") {
        let mut a = input.split("in");
        match a.next() {
            Some(number) => {
                let n: u32 = number.parse().map_err(|_| "height bad 4")?;
                // If in, the number must be at least 59 and at most 76.
                if n >= 59 && n <= 76 {
                    Ok(())
                } else {
                    Err("height in range incorrect")
                }
            },
            None => Err("height bad 3")
        }
    } else {
        Err("invalid height units")
    }
}

fn validate_birth_year(input: &str) -> Result<(), &'static str> {
    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    let birth_year: u32 = input.parse()
        .map_err(|_| "could not parse birth year as u32")?;

    if birth_year >= 1920 && birth_year <= 2002 {
        Ok(())
    } else {
        Err("Birth year not in range")
    }
}

fn validate_issue_year(input: &str) -> Result<(), &'static str> {
    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    let birth_year: u32 = input.parse()
        .map_err(|_| "could not parse birth year as u32")?;

    if birth_year >= 2010 && birth_year <= 2020 {
        Ok(())
    } else {
        Err("Issue year not in range")
    }
}

fn validate_expiration_year(input: &str) -> Result<(), &'static str> {
    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    let birth_year: u32 = input.parse()
        .map_err(|_| "could not parse birth year as u32")?;

    if birth_year >= 2020 && birth_year <= 2030 {
        Ok(())
    } else {
        Err("Expiration year not in range")
    }
}

fn solve2(input: &[HashMap<&str, &str>]) -> usize {
    input.iter()
        .filter(|p| is_passport_valid(*p))
        .filter(|p| match is_passport_valid2(*p) {
            Ok(()) => true,
            Err(_) => false,
        })
        .count()
}

fn parse_input(input: &str) -> Vec<HashMap<&str, &str>> {
    let (_, passports) = separated_list1(multispace1, parse_passport)(input).unwrap();

    passports
}

fn parse_passport(input: &str) -> IResult<&str, HashMap<&str, &str>> {
    let (input, pairs) = separated_list1(alt((tag(" "), line_ending)), parse_pair)(input)?;

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

    #[test]
    fn test_invalid_example_part2() {
        let input = include_str!("invalid_example");
        let input = parse_input(input);

        let expected = 0;
        let actual = solve2(&input);

        assert_eq!(expected, actual)
    }

    #[test]
    fn test_valid_example_part2() {
        let input = include_str!("valid_example");
        let input = parse_input(input);

        let expected = 4;
        let actual = solve2(&input);

        assert_eq!(expected, actual)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 188;
        let actual = solve2(&input);

        assert_eq!(expected, actual)
    }
}