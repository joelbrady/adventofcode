use std::collections::HashSet;

use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::IResult;
use nom::multi::separated_list1;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    cubes: Vec<(i64, i64, i64)>,
}

fn parse_input(s: &str) -> Input {
    let (_, cubes) = separated_list1(line_ending, parse_cube)(s).unwrap();

    Input { cubes }
}

fn parse_cube(s: &str) -> IResult<&str, (i64, i64, i64)> {
    let (rem, a) = nom::character::complete::i64(s)?;
    let (rem, _) = tag(",")(rem)?;
    let (rem, b) = nom::character::complete::i64(rem)?;
    let (rem, _) = tag(",")(rem)?;
    let (rem, c) = nom::character::complete::i64(rem)?;

    Ok((rem, (a, b, c)))
}

fn solve_part1(input: &Input) -> i64 {
    let dirs = dirs();
    let cubes: HashSet<_> = input.cubes.iter().copied().collect();
    cubes.iter()
        .map(|(x, y, z)| {
            let sides_covered = dirs.iter()
                .map(|(dx, dy, dz)| (x + dx, y + dy, z + dz))
                .map(|(x, y, z)| i64::from(cubes.contains(&(x, y, z))))
                .sum::<i64>();
            6 - sides_covered
        })
        .sum()
}

fn dirs() -> Vec<(i64, i64, i64)> {
    vec![
        (-1, 0, 0),
        (1, 0, 0),
        (0, -1, 0),
        (0, 1, 0),
        (0, 0, -1),
        (0, 0, 1),
    ]
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 64;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 4242;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}