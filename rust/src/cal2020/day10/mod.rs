use std::collections::HashSet;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

fn parse_input(input: &str) -> Vec<u32> {
    input.lines()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn solve(adapters: &[u32]) -> u32 {
    let adapters = prepare(adapters);

    let mut plus1 = 0;
    let mut plus3 = 0;

    for i in 0..(adapters.len() - 1) {
        let a = adapters[i];
        let b = adapters[i + 1];

        if (b - a) == 1 {
            plus1 += 1;
        }

        if (b - a) == 3 {
            plus3 += 1;
        }
    }

    plus1 * plus3
}

fn prepare(adapters: &[u32]) -> Vec<u32> {
    let max = adapters.iter().max().unwrap();
    let mut adapters: Vec<u32> = adapters.iter().copied().collect();
    adapters.push(*max + 3);
    adapters.push(0);

    adapters.sort_unstable();

    adapters
}

fn solve2(input: &[u32]) -> usize {
    let input = prepare(input);
    let mut removable: Vec<bool> = (0..input.len()).map(|_| true).collect();

    // the ends can't be removed
    removable[0] = false;
    removable[input.len() - 1] = false;

    // neighbours of distance 3 can't be removed (eg .., 4, 7, ..)
    for i in 1..(input.len() - 2) {
        let a = input[i];
        let b = input[i + 1];
        if (b - a) == 3 {
            removable[i] = false;
            removable[i + 1] = false;
        }
    }

    let mut ways = 1;

    let mut i = 0;

    while i < (input.len() - 1) {
        if !removable[i] {
            for j in (i+1)..(input.len()) {
                if !removable[j] {
                    if j == i + 1 {
                        i = j;
                        break;
                    }
                    let slice = &input[i..j+1];
                    ways *= combinations(slice).len();
                    i = j;
                    break;
                }
            }
        }
    }

    ways
}

// input slice includes start + end that can't be removed
fn combinations(slice: &[u32]) -> HashSet<Vec<u32>> {
    let mut ways: HashSet<Vec<u32>> = HashSet::new();

    if valid(slice) {
        ways.insert(slice.iter().copied().collect());
        for i in 1..(slice.len() - 1) {
            let mut copy: Vec<u32> = slice.iter().copied().collect();
            copy.remove(i);
            let others = combinations(&copy);
            for o in others.into_iter() {
                ways.insert(o);
            }
        }
    }

    ways
}

fn valid(input: &[u32]) -> bool {
    for i in 0..(input.len() - 1) {
        let d = input[i + 1] - input[i];
        if d > 3 {
            return false;
        }
    }

    true
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

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 8;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example2_part2() {
        let input = include_str!("example2");
        let input = parse_input(input);

        let expected = 19208;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected: usize = 453551299002368;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_combinations() {
        let a = [4, 5, 6, 7];

        let expected = 4;
        let actual = combinations(&a).len();

        assert_eq!(actual, expected)
    }
}