use crate::cal2019::intcode::{Machine, parse_program, StoppedState};

pub fn main() {
    let input = include_str!("input");
    let solution = solve(&input);

    println!("The solution to part 1 is {}", solution);

    let part2 = solve2(&input);

    println!("The solution to part 2 is {}", part2);
}

fn permute<T>(ts: Vec<T>) -> Vec<Vec<T>>
    where T: Clone {
    if ts.len() == 1 {
        return vec![ts];
    }

    let mut permutations: Vec<Vec<T>> = vec![];

    for start in 0..ts.len() {
        let mut ts_ = ts.clone();
        let t = ts_.remove(start);
        let ps: Vec<Vec<T>> = permute(ts_);
        for p in ps {
            let mut p_: Vec<T> = vec![t.clone()];
            p_.extend(p);
            permutations.push(p_);
        }
    }

    permutations
}

fn solve(s: &str) -> i64 {
    let program = parse_program(s);
    let sequences: Vec<Vec<i64>> = permute(vec![0, 1, 2, 3, 4]);
    sequences
        .iter()
        .map(|s| run_sequence(&program, s))
        .max()
        .unwrap()
}

fn solve2(s: &str) -> i64 {
    let program = parse_program(s);
    let sequences: Vec<Vec<i64>> = permute(vec![5, 6, 7, 8, 9]);
    sequences
        .iter()
        .map(|s| run_feedback_sequence(&program, s))
        .max()
        .unwrap()
}

fn run_sequence(program: &[i64], sequence: &[i64]) -> i64 {
    let mut signal = 0;
    for seq in sequence {
        let mut m = Machine::new_test_mode(program, &vec![*seq, signal]);
        if let StoppedState::Halted = m.run() {
            signal = m.output();
        } else {
            panic!("unexpected stop state");
        }
    }
    signal
}

fn run_feedback_sequence(program: &[i64], sequence: &[i64]) -> i64 {
    let mut ms: Vec<Box<Machine>> = vec![];
    for i in 0..5 {
        let mut m = Machine::new_feedback_mode(program);
        m.input(sequence[i]);
        ms.push(Box::new(m));
    }

    let mut input = 0;
    let mut i = 0;
    loop {
        let machine_id = i % 5;
        let m = ms[machine_id].as_mut();
        m.input(input);
        let state = m.run();
        match state {
            StoppedState::BlockedOnInput => {
                input = m.output();
                i += 1;
                continue
            },
            StoppedState::Halted => {
                if machine_id != 4 {
                    input = m.output();
                    i += 1;
                    continue
                } else {
                    return m.output()
                }
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example_1() {
        let input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
        let input = parse_program(input);
        let sequence = vec![4, 3, 2, 1, 0];
        let expected = 43210;
        assert_eq!(run_sequence(&input, &sequence), expected);
    }

    #[test]
    fn test_example_2() {
        let input = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";
        let input = parse_program(input);
        let sequence = vec![0, 1, 2, 3, 4];
        let expected = 54321;
        assert_eq!(run_sequence(&input, &sequence), expected);
    }

    #[test]
    fn test_example_3() {
        let input = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0";
        let input = parse_program(input);
        let sequence = vec![1, 0, 4, 3, 2];
        let expected = 65210;
        assert_eq!(run_sequence(&input, &sequence), expected);
    }

    #[test]
    fn test_permute() {
        let a = vec![1, 2, 3];
        let expected = vec![vec![1, 2, 3], vec![1, 3, 2], vec![3, 2, 1], vec![3, 1, 2], vec![2, 1, 3], vec![2, 3, 1]];
        let permutations: Vec<Vec<i64>> = permute(a);
        for v in expected {
            assert!(permutations.contains(&v), "{:?}", v);
        }
    }

    #[test]
    fn test_part2_example1() {
        let input = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
        let input = parse_program(input);
        let sequence = vec![9, 8, 7, 6, 5];

        let expected = 139629729;
        let signal = run_feedback_sequence(&input, &sequence);
        assert_eq!(signal, expected);
    }

    #[test]
    fn test_part2_example2() {
        let input = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";
        let input = parse_program(input);
        let sequence = vec![9, 7, 8, 5, 6];

        let expected = 18216;
        let signal = run_feedback_sequence(&input, &sequence);
        assert_eq!(signal, expected);
    }

    #[test]
    fn test_solutions() {
        let input = include_str!("input");
        let solution = solve(&input);

        assert_eq!(solution, 87138);

        let solution = solve2(&input);

        assert_eq!(solution, 17279674);
    }
}
