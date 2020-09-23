fn main() {
    let input = include_str!("input");
    println!("The solution to part 1 is {}", solve_part1(&input));
    println!("The solution to part 2 is {}", solve_part2(&input));
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

fn solve_part2(input: &str) -> String {
    let input = parse_input(input);
    let input = input.repeat(10000);
    // TODO parse offset from file
    let result = fft_part2(&input, 100, 5971723);
    let a: Vec<String> = result.iter().skip(5971723).take(8).map(|n| n.to_string()).collect();
    a.join("")
}

fn parse_input(s: &str) -> Vec<i32> {
    s.trim().chars()
        .map(|c| c.to_digit(10).unwrap() as i32)
        .collect()
}

fn fft(input: &[i32], iterations: usize) -> Vec<i32> {
    let mut a: Vec<i32> = input.iter().copied().collect();
    let mut b: Vec<i32> = a.to_vec();

    for _ in 0..iterations {
        fast_round(&a, &mut b);
        std::mem::swap(&mut a, &mut b);
    }

    a
}

fn fft_part2(input: &[i32], iterations: usize, offset: usize) -> Vec<i32> {
    let mut a: Vec<i32> = input.iter().copied().collect();

    for _ in 0..iterations {
        fast_round2(&mut a, offset);
    }

    a
}

fn fast_round2(a: &mut [i32], offset: usize) {
    for i in (offset..a.len() - 1).rev() {
        a[i] += a[i + 1];
    }

    for i in (offset..a.len()).rev() {
        a[i] %= 10;
    }
}

fn fast_round(input: &[i32], output: &mut [i32]) {
    for idx in 1..=input.len() {
        output[idx - 1] = dot(input, idx).abs() % 10
    }
}

fn x(position: usize, element: usize) -> i32 {
    let repeats_at = 4 * position;
    let element = element + repeats_at;
    let element = element + 1;
    let section = element / position;
    let section = section % 4;
    z(section)
}

fn z(i: usize) -> i32 {
    match i {
        0 => 0,
        1 => 1,
        2 => 0,
        3 => -1,
        _ => panic!()
    }
}

fn dot(a: &[i32], idx: usize) -> i32 {
    a.iter()
        .enumerate()
        .map(|(i, n)|  n * x(idx, i))
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;

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

    #[test]
    fn test_x() {
        assert_eq!(x(1, 0), 1);
        assert_eq!(x(1, 1), 0);
        assert_eq!(x(1, 2), -1);
        assert_eq!(x(1, 3), 0);

        assert_eq!(x(2, 0), 0);
        assert_eq!(x(2, 1), 1);
        assert_eq!(x(2, 2), 1);
        assert_eq!(x(2, 3), 0);
        assert_eq!(x(2, 4), 0);
        assert_eq!(x(2, 5), -1);
        assert_eq!(x(2, 6), -1);
        assert_eq!(x(2, 7), 0);
    }

    #[test]
    fn part_one() {
        let input = include_str!("input");
        let solution = solve_part1(input);

        assert_eq!(solution, "58672132")
    }

    #[test]
    fn part_two() {
        let input = include_str!("input");
        let solution = solve_part2(input);

        assert_eq!(solution, "91689380")
    }
}