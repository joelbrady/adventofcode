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
    crab_positions: Vec<i64>,
}

fn parse_input(s: &str) -> Input {
    let crab_positions = s.split(',')
        .map(|n| n.parse().unwrap())
        .collect();

    Input {
        crab_positions,
    }
}

fn solve(input: &Input) -> i64 {
    let crabs: &[i64] = &input.crab_positions;
    let min = *crabs.iter().min().unwrap();
    let max = *crabs.iter().max().unwrap();

    (min..=max)
        .map(|target_distance| cost_to_target_distance(crabs, target_distance))
        .min()
        .unwrap()
}

fn cost_to_target_distance(crabs: &[i64], target_distance: i64) -> i64 {
    crabs.iter()
        .map(|c| (target_distance - c).abs())
        .sum()
}

fn solve2(input: &Input) -> i64 {
    let crabs: &[i64] = &input.crab_positions;
    let min = *crabs.iter().min().unwrap();
    let max = *crabs.iter().max().unwrap();

    (min..=max)
        .map(|target_distance| cost_to_target_distance2(crabs, target_distance))
        .min()
        .unwrap()
}

fn cost_to_target_distance2(crabs: &[i64], target_distance: i64) -> i64 {
    crabs.iter()
        .map(|c| (target_distance - c).abs())
        .map(|d| (d * (d + 1)) / 2)
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 37;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 352997;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 168;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 101571302;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}