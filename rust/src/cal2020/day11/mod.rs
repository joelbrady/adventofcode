pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Tile {
    EmptySeat,
    Floor,
    OccupiedSeat,
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            'L' => Tile::EmptySeat,
            '#' => Tile::OccupiedSeat,
            '.' => Tile::Floor,
            _ => unimplemented!(),
        }
    }
}

fn parse_input(input: &str) -> Vec<Vec<Tile>> {
    input.lines()
        .map(|s| s.chars().map(|t| t.into()).collect())
        .collect()
}

// fn display(buffer: &[Vec<Tile>]) {
//     for row in buffer.iter() {
//         for col in row.iter() {
//             match *col {
//                 Tile::EmptySeat => print!("L"),
//                 Tile::Floor => print!("."),
//                 Tile::OccupiedSeat => print!("#"),
//             }
//         }
//         println!();
//     }
//     println!();
// }

// If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
// If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
// Otherwise, the seat's state does not change.
//
// Floor (.) never changes; seats don't move, and nobody sits on the floor.

fn solve(input: &[Vec<Tile>]) -> usize {
    generic_solution(input, 4, get_around_1)
}

fn solve2(input: &[Vec<Tile>]) -> usize {
    generic_solution(input, 5, get_around_inf)
}

fn generic_solution<F>(input: &[Vec<Tile>], limit: usize, adjacency_check: F) -> usize
    where F: Fn(i32, i32, &[Vec<Tile>]) -> usize {
    let a: Vec<Vec<Tile>> = input.to_vec();
    let rows = a.len();
    let cols = a[0].len();
    let mut buffers = [a.clone(), a];

    let mut i = 0;

    loop {
        let input_buffer = i % 2;
        let output_buffer = (i + 1) % 2;
        for row in 0..rows {
            for col in 0..cols {
                let adjacent = adjacency_check(row as i32, col as i32, &(buffers[input_buffer]));
                let this = buffers[input_buffer][row][col];
                match this {
                    Tile::EmptySeat => {
                        if adjacent == 0 {
                            buffers[output_buffer][row][col] = Tile::OccupiedSeat;
                            continue;
                        }
                    }
                    Tile::OccupiedSeat => {
                        if adjacent >= limit {
                            buffers[output_buffer][row][col] = Tile::EmptySeat;
                            continue;
                        }
                    }
                    _ => {},
                }
                buffers[output_buffer][row][col] = buffers[input_buffer][row][col];
            }
        }

        if buffers[input_buffer] == buffers[output_buffer] {
            break;
        }

        i += 1;
    }

    buffers[0].iter()
        .map(|row| row.iter().filter(|t| **t == Tile::OccupiedSeat).count())
        .sum()
}

fn get_around_1(row: i32, col: i32, m: &[Vec<Tile>]) -> usize {
    let potentials = [(row - 1, col - 1), (row, col - 1), (row + 1, col - 1), (row + 1, col), (row + 1, col + 1), (row, col + 1), (row - 1, col + 1), (row - 1, col)];
    let max_row = (m.len() - 1) as i32;
    let max_col = (m[0].len() - 1) as i32;

    potentials.iter()
        .filter(|(row, col)| *row >= 0 && *row <= max_row && *col >= 0 && *col <= max_col)
        .map(|(row, col)| m[(*row) as usize][(*col) as usize])
        .filter(|tile| *tile == Tile::OccupiedSeat)
        .count()
}

fn get_around_inf(row: i32, col: i32, m: &[Vec<Tile>]) -> usize {
    let directions = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)];
    let max_row = (m.len() - 1) as i32;
    let max_col = (m[0].len() - 1) as i32;

    let mut occupied = 0;

    for (row_delta, col_delta) in directions.iter() {
        let mut row = row + row_delta;
        let mut col = col + col_delta;
        while row >= 0 && row <= max_row && col >= 0 && col <= max_col {
            let tile = m[row as usize][col as usize];
            if tile == Tile::OccupiedSeat {
                occupied += 1;
                break;
            }
            if tile == Tile::EmptySeat {
                break;
            }
            row += row_delta;
            col += col_delta;
        }
    }

    occupied
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 37;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 2126;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 26;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1914;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}