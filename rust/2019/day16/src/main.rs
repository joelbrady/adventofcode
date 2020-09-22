use std::collections::HashMap;
use input::get_input;

fn main() {
    let input = get_input("2019/day16/input");
    println!("The solution to part 1 is {}", solve_part1(&input));
}

fn solve_part1(input: &str) -> String {
    let input = parse_input(input);
    let result: Vec<String> = fft(&input, 100)
        .iter()
        .take(8)
        .copied()
        .map(|n| n.to_string())
        .collect();

    result.join("")
}

fn parse_input(s: &str) -> Vec<i32> {
    s.trim().chars()
        .map(|c| c.to_digit(10).unwrap() as i32)
        .collect()
}

fn fft(input: &[i32], iterations: usize) -> Vec<i32> {
    let patterns = build_patterns(input.len() + 1);
    let mut a: Vec<i32> = input.iter().copied().collect();

    for _ in 0..iterations {
        let b = round(&a, &patterns);
        a = b;
    }

    a
}

fn round(input: &[i32], patterns: &HashMap<usize, Vec<i32>>) -> Vec<i32> {
    (1..=input.len())
        .map(|idx| {
            let pattern = patterns.get(&idx).unwrap();
            dot(input, pattern).abs() % 10
        })
        .collect()
}

fn dot(a: &[i32], b: &[i32]) -> i32 {
    a.iter()
        .zip(b.iter())
        .map(|(a, b)|  (a * b))
        .sum()
}

fn base() -> Vec<i32> {
    vec![0, 1, 0, -1]
        .repeat(10000)
}

fn build_patterns(max_index: usize) -> HashMap<usize, Vec<i32>> {
    (1..max_index)
        .map(|idx| (idx, pattern(idx, max_index)))
        .collect()
}

fn pattern(n: usize, max_length: usize) -> Vec<i32> {
    let b = base();
    duplicate(&b, n, max_length)
        .iter()
        .skip(1)
        .take(max_length)
        .copied()
        .collect()
}

fn duplicate(v: &[i32], n: usize, max_length: usize) -> Vec<i32> {
    v.iter()
        .flat_map(|i| vec![i].repeat(n))
        .take(max_length + 1)
        .copied()
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn duplicate_test() {
        let v = vec![1, 2, 3];
        let expected = vec![1, 1, 1, 2, 2, 2, 3, 3, 3];
        let actual = duplicate(&v, 3, expected.len());

        assert_eq!(actual, expected)
    }

    #[test]
    fn pattern_test() {
        let expected = vec![0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1];
        let actual: Vec<i32> = pattern(2, expected.len()).iter()
            .take(expected.len())
            .copied()
            .collect();

        assert_eq!(actual, expected)
    }

    #[test]
    fn fft_example() {
        let expected1 = vec![4, 8, 2, 2, 6, 1, 5, 8];
        let input = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let actual1 = fft(&input, 1);

        assert_eq!(actual1, expected1);

        let expected2 = vec![3, 4, 0, 4, 0, 4, 3, 8];
        let actual2 = fft(&input, 2);

        assert_eq!(actual2, expected2);

        let expected4 = vec![0, 1, 0, 2, 9, 4, 9, 8];
        let actual4 = fft(&input, 4);

        assert_eq!(actual4, expected4)
    }

    #[test]
    fn test_parse_input() {
        let input = "123";
        let expected = vec![1, 2, 3];

        let actual = parse_input(input);
        assert_eq!(actual, expected)
    }

    #[test]
    fn fft_bigger_examples() {
        let input = parse_input("80871224585914546619083218645595");
        let expected = parse_input("24176176");

        let actual: Vec<i32> = fft(&input, 100)
            .iter()
            .take(8)
            .copied()
            .collect();

        assert_eq!(actual, expected)
    }
}