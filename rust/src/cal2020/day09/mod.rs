pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input, 25);

    println!("The solution to part 1 is {}", part1);
    // let part2 = solve2(&input);

    // println!("The solution to part 2 is {}", part2);
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

        let expected = 0;
        let actual = solve(&input, 25);

        assert_eq!(actual, expected)
    }
}