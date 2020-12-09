pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input, 25);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input, 25);
    println!("The solution to part 2 is {}", part2);
}

fn parse_input(input: &str) -> Vec<i64> {
    input.lines()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn solve(input: &[i64], preamble_length: usize) -> i64 {
    let mut queue = vec![];
    for n in input.iter() {
        if queue.len() == preamble_length && !is_sum_of_two_numbers(&queue, *n) {
            return *n;
        }
        queue.push(*n);
        if queue.len() > preamble_length {
            queue.remove(0);
        }
    }

    unimplemented!()
}

fn is_sum_of_two_numbers(numbers: &[i64], n: i64) -> bool {
    for a in numbers.iter() {
        for b in numbers.iter() {
            if a != b && a + b == n {
                return true;
            }
        }
    }

    false
}

fn solve2(input: &[i64], preamble_length: usize) -> i64 {
    let part1_solution = solve(input, preamble_length);
    let (start, end): (usize, usize) = find_contiguous_range(input, part1_solution);

    let subslice = &input[start..end];

    subslice.iter().max().unwrap() + subslice.iter().min().unwrap()
}

fn find_contiguous_range(input: &[i64], target_sum: i64) -> (usize, usize) {
    for range_size in 2..=input.len() {
        for start in 0..(input.len() - range_size) {
            let end = start + range_size;
            let subslice: &[i64] = &input[start..end];
            let sum: i64 = subslice.iter().sum();
            if sum == target_sum {
                return (start, end);
            }
        }
    }

    unimplemented!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 127;
        let actual = solve(&input, 5);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 41682220;
        let actual = solve(&input, 25);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 62;
        let actual = solve2(&input, 5);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 5388976;
        let actual = solve2(&input, 25);

        assert_eq!(actual, expected)
    }
}