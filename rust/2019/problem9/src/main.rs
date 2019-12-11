use input::get_input;
use intcode::{parse_program, Machine};

fn main() {
    let input = get_input("2019/problem9/input");
    let program = parse_program(&input);
    let mut m = Machine::new_test_mode(&program, &vec![1]);
    m.run();

    println!("The solution to part 1 is {}", m.output());
}

#[cfg(test)]
mod test {
    use super::*;
    use intcode::{parse_program, Machine};

    #[test]
    fn test_example1() {
        let input = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";
        let input = parse_program(input);
        let mut m = Machine::new_feedback_mode(&input);
        m.run();
        let output = m.dump_output_buffer();
        assert_eq!(output, input);
    }

    #[test]
    fn test_example2() {
        let input = "1102,34915192,34915192,7,4,7,99,0";
        let input = parse_program(input);
        let mut m = Machine::new_feedback_mode(&input);
        m.run();
        let output = m.output();
        assert_eq!(output, 34915192 * 34915192);
    }

    #[test]
    fn test_example3() {
        let input = "104,1125899906842624,99";
        let input = parse_program(input);
        let mut m = Machine::new_feedback_mode(&input);
        m.run();
        let output = m.output();
        assert_eq!(output, 1125899906842624)
    }

    #[test]
    fn test_solution_part1() {
        let expected = 4288078517;
        let input = get_input("input");
        let program = parse_program(&input);
        let mut m = Machine::new_test_mode(&program, &vec![1]);
        m.run();

        assert_eq!(expected, m.output());
    }
}
