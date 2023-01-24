pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    numbers: Vec<i64>,
}

fn parse_input(s: &str) -> Input {
    let numbers = s.lines()
        .map(|l| l.parse().unwrap())
        .collect();

    Input { numbers }
}

#[derive(Debug)]
struct OriginalLocation(usize);

fn solve_part1(input: &Input) -> i64 {
    let mut v: Vec<_> = input.numbers.iter()
        .enumerate()
        .map(|(i, n)| (OriginalLocation(i), *n))
        .collect();


    for i in 0..(v.len()) {
        let (mut current_index, (_, n)) = v.iter()
            .enumerate()
            .find(|(_, (original_index, _n))| original_index.0 == i)
            .unwrap();
        let increment: i64 = if *n > 0 { 1 } else { -1 };

        let n = *n;
        let n_abs = n.abs();

        for _ in 0..n_abs {
            let mut next = (current_index as i64) + increment;
            if next >= v.len() as i64 {
                next = 0;
            } else if next < 0 {
                next = v.len() as i64 - 1;
            }
            v.swap(current_index, next as usize);
            current_index = next as usize;

            // print(&v);
        }
    }

    let (zeros_index, _) = v.iter()
        .enumerate()
        .find(|(_i, (_, n))| *n == 0)
        .unwrap();

    [1000, 2000, 3000].into_iter()
        .map(|i| i + zeros_index)
        .map(|i| v[i % v.len()].1)
        .sum()
}

#[allow(dead_code)]
fn print(v: &[(OriginalLocation, i64)]) {
    print!("[");
    for n in v.iter()
        .map(|(_, b)| *b) {
        print!("{n}, ");
    }
    println!("]");
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 3;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 8764;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}