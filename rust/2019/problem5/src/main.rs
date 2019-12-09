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
    let mut m = Machine::new(&parsed_input, &vec![1]);
    let state = m.run();
    match state {
        StoppedState::Halted(output) => output,
        _ => unimplemented!()
    }
}

fn solve2(s: &str) -> i32 {
    let parsed_input = parse_program(s);
    let mut m = Machine::new(&parsed_input, &vec![5]);
    let state = m.run();
    match state {
        StoppedState::Halted(output) => output,
        _ => unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solutions() {
        let input = get_input("input");
        let solution = solve(&input);

        assert_eq!(solution, 6731945);

        let part_2 = solve2(&input);

        assert_eq!(part_2, 9571668);
    }
}
