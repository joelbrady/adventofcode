pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Eq, PartialEq)]
struct Input {}

fn parse_input(_s: &str) -> Input {
    Input {}
}

fn solve_part1(_input: &Input) -> i64 {
    todo!()
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_input() {
        parse_input("");
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 157;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 8053;
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