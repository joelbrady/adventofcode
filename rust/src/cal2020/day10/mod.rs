use std::collections::HashSet;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    // let part2 = solve2(&input, 25);
    // println!("The solution to part 2 is {}", part2);
}

fn parse_input(input: &str) -> Vec<u32> {
    input.lines()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn solve(adapters: &[u32]) -> u32 {
    let max = adapters.iter().max().unwrap();
    let mut adapters: HashSet<u32> = adapters.iter().copied().collect();
    adapters.insert(*max + 3);

    let mut current: u32 = 0;
    let mut plus1 = 0;
    let mut plus3 = 0;
    while !adapters.is_empty() {
        let candidates = adapters.iter().filter(|a| **a <= current + 3);
        let smallest = *candidates.min().unwrap();
        adapters.remove(&smallest);
        if smallest == current + 1 {
            plus1 += 1;
        } else if smallest == current + 3 {
            plus3 += 1;
        }
        current = smallest;
    }

    plus1 * plus3
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 7 * 5;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 2432;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }
}