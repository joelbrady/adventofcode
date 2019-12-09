use input::get_input;
use intcode::{parse_program, Machine, StoppedState};

fn main() {
    let input = get_input("input");
    let solution = solve(&input);

    println!("The solution to part 1 is {}", solution);

//    let part2 = solve2(&input);

//    println!("The solution to part 2 is {}", part2);
}

fn permute<T>(ts: Vec<T>) -> Vec<Vec<T>>
    where T: Clone {

    if ts.len() == 1 {
        return vec![ts]
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

fn solve(s: &str) -> i32 {
    let program = parse_program(s);
    let sequences: Vec<Vec<i32>> = permute(vec![0, 1, 2, 3, 4]);
    sequences
        .iter()
        .map(|s| run_sequence(&program, s))
        .max()
        .unwrap()
}

fn run_sequence(program: &[i32], sequence: &[i32]) -> i32 {
    let mut signal = 0;
    for seq in sequence {
        let mut m = Machine::new(program, &vec![*seq, signal]);
        if let StoppedState::Halted(output) = m.run() {
            signal = output;
        } else {
            panic!("unexpected stop state");
        }
    }
    signal
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
        let permutations: Vec<Vec<i32>> = permute(a);
        for v in expected {
            assert!(permutations.contains(&v), "{:?}", v);
        }
    }

    #[test]
    fn test_solutions() {
        let input = get_input("input");
        let solution = solve(&input);

        assert_eq!(solution, 87138);
    }
}
