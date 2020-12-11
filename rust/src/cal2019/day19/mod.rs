use crate::cal2019::intcode::{Machine, parse_program};

pub fn main() {
    let input = include_str!("input");
    let input = parse_program(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

fn solve(program: &[i64]) -> i64 {
    let mut sum = 0;

    for row in 0..50 {
        for col in 0..50 {
            let mut m = Machine::new_feedback_mode(program);
            m.input(col);
            m.input(row);
            m.run();
            sum += m.output();
        }
    }

    sum
}

fn solve2(program: &[i64]) -> usize {
    unimplemented!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_program(input);

        let expected = 194;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_program(input);

        let expected = 0;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}