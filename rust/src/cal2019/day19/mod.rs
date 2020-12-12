use crate::cal2019::intcode::{Machine, parse_program};

pub fn main() {
    let input = include_str!("input");
    let input = parse_program(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    solve2(&input);
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

fn solve2(program: &[i64]) {
    let x_start = 1011;
    let y_start = 555;
    let dimension = 100;
    let mut grid = vec![];

    for row in y_start..(y_start + dimension) {
        let mut row_vec = vec![];
        for col in x_start..(x_start + dimension) {
            let mut m = Machine::new_feedback_mode(program);
            m.input(col as i64);
            m.input(row as i64);
            m.run();
            let n = m.output() == 1;
            row_vec.push(n);
        }
        grid.push(row_vec);
    }

    display(&grid);

}

fn display(grid: &[Vec<bool>]) {
    for row in grid.iter() {
        for tile in row.iter() {
            match tile {
                true => print!("#"),
                false => print!("."),
            }
        }
        println!();
    }
    println!();
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

        // solve visually
        solve2(&input);
    }
}