pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

struct Input {
    depths: Vec<i64>,
}

fn parse_input(s: &str) -> Input {
    let depths = s.lines()
        .map(|l| l.parse().unwrap())
        .collect();

    Input {
        depths
    }
}

fn solve(input: &Input) -> usize {
    let ns = &input.depths;

    ns.iter()
        .zip(ns.iter().skip(1))
        .filter(|(a, b)| a < b)
        .count()
}

fn solve2(input: &Input) -> usize {
    let ns = &input.depths;

    let ns: Vec<i64> = ns.iter()
        .zip(ns.iter().skip(1))
        .zip(ns.iter().skip(2))
        .map(|((a, b), c)| a + b + c)
        .collect();

    solve(&Input { depths: ns })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_input() {
        let input = include_str!("example1");
        let actual = parse_input(input);
        let expected = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];

        assert_eq!(actual.depths, expected)
    }

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 7;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1583;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 5;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1627;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}