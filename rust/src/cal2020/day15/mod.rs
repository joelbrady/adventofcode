use std::collections::HashMap;

pub fn main() {
    let input = vec![1, 0, 18, 10, 19, 6];

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

fn solve(input: &[u64]) -> u64 {
    let target_index = 2020;

    solve_any(input, target_index)
}

fn solve2(input: &[u64]) -> u64 {
    let target_index = 30000000;

    solve_any(input, target_index)
}

fn solve_any(input: &[u64], target_index: u64) -> u64 {
    assert!(!input.is_empty());

    let mut seen = HashMap::new();

    let mut last = input[input.len() - 1];

    for (i, n) in input.iter().enumerate() {
        seen.insert(*n, vec![(i as u64) + 1]);
    }

    seen.remove(&last);

    let mut count = input.len() as u64;

    while count < target_index {
        let previous_occurrences = seen.get(&last);

        let most_recent = previous_occurrences.map(|a| a.last().unwrap());
        let difference = most_recent.map(|b| count - *b).unwrap_or(0);

        seen.entry(last).or_insert_with(Vec::new).push(count);
        last = difference;

        count += 1;
    }

    last
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = vec![0, 3, 6];

        let expected = 436;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = vec![1, 0, 18, 10, 19, 6];

        let expected = 441;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = vec![1, 0, 18, 10, 19, 6];

        let expected = 10613991;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}