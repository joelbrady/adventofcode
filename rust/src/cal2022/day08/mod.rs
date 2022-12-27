pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Eq, PartialEq)]
struct Input {
    trees: Vec<Vec<u32>>,
}

fn parse_input(s: &str) -> Input {
    let trees = s.lines()
        .map(|line| line.chars()
            .map(|c| c.to_digit(10).unwrap())
            .collect())
        .collect();
    Input { trees }
}

fn solve_part1(input: &Input) -> u32 {
    let n_rows = input.trees.len() as i32;
    let n_cols = input.trees[0].len() as i32;

    let mut can_be_seen = 0;

    for row in 0..n_rows {
        for col in 0..n_cols {
            if check(row, col, input) {
                can_be_seen += 1;
            }
        }
    }

    can_be_seen
}

fn check(row: i32, col: i32, input: &Input) -> bool {
    let trees = &input.trees;

    let max_row = (trees.len() - 1) as i32;
    let max_col = (trees[0].len() - 1) as i32;

    if row == 0 || col == 0 || row == max_row || col == max_col {
        return true;
    }

    let bounds = (max_row, max_col);

    let tree = trees[row as usize][col as usize];

    let a = [(1, 0), (0, 1), (-1, 0), (0, -1)]
        .map(|(dy, dx)| {
            let mut pos = (row + dy, col + dx);
            while in_bounds(pos, bounds) {
                let other = trees[pos.0 as usize][pos.1 as usize];
                if other >= tree {
                    // dbg!(row, col, &pos, tree, other);
                    return false;
                }
                pos = (pos.0 + dy, pos.1 + dx);
            }
            true
        });

    a.contains(&true)
}

fn in_bounds((y, x): (i32, i32), (max_row, max_col): (i32, i32)) -> bool {
    y >= 0 && y <= max_row && x >= 0 && x <= max_col
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
        let expected = 21;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 1679;
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