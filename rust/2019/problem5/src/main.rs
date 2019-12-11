use intcode::{parse_program, Machine, StoppedState};
use input::get_input;

fn main() {
    let input = get_input("input");
    let solution = solve(&input);

    println!("The solution to part 1 is {}", solution);

    let part2 = solve2(&input);

    println!("The solution to part 2 is {}", part2);
}

fn solve(s: &str) -> i32 {
    let parsed_input = parse_program(s);
    let mut m = Machine::new_test_mode(&parsed_input, &vec![1]);
    let state = m.run();
    match state {
        StoppedState::Halted => m.output(),
        _ => unimplemented!()
    }
}

fn solve2(s: &str) -> i32 {
    let parsed_input = parse_program(s);
    let mut m = Machine::new_feedback_mode(&parsed_input);
    m.input(5);
    let state = m.run();
    match state {
        StoppedState::Halted => m.output(),
        _ => unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution_part1() {
        let input = get_input("input");
        let solution = solve(&input);

        assert_eq!(solution, 6731945);
    }

    #[test]
    fn test_solution_part2() {
        let input = get_input("input");
        let part_2 = solve2(&input);

        assert_eq!(part_2, 9571668);
    }
}
