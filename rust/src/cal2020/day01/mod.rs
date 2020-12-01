pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

fn solve(input: &[i32]) -> i32 {
    input.iter()
        .flat_map(move |a| input.iter()
            .map(move |b| (*a, *b))
        )
        .map(|(a, b)| ((a, b), a + b))
        .filter(|(_, sum)| *sum == 2020)
        .map(|((a, b), _)| a * b)
        .take(1)
        .sum()
}

fn solve2(input: &[i32]) -> i32 {
    input.iter()
        .flat_map(move |a| input.iter()
            .flat_map(move |b| input.iter().map(move |c| (*a, *b, *c))))
        .filter(|(a, b, c)| (a + b + c) == 2020)
        .map(|(a, b, c)| a * b * c)
        .take(1)
        .sum()
}

fn parse_input(input: &str) -> Vec<i32> {
    input.split_ascii_whitespace()
        .map(|s| s.parse().expect("input can be parsed as i32"))
        .collect()
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 514579;
        let actual = solve(&input);

        assert_eq!(expected, actual)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 744475;
        let actual = solve(&input);

        assert_eq!(expected, actual)
    }

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 241861950;
        let actual = solve2(&input);

        assert_eq!(expected, actual)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 70276940;
        let actual = solve2(&input);

        assert_eq!(expected, actual)
    }
}