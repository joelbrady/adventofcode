pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(input.clone());

    println!("The solution to part 1 is {}", part1);

    // let part2 = solve2(&input);
    // println!("The solution to part 2 is {}", part2);
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


// If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
// If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
// Otherwise, the seat's state does not change.
//
// Floor (.) never changes; seats don't move, and nobody sits on the floor.

fn solve(input: Vec<Vec<Tile>>) -> usize {
    let mut a: Vec<Vec<Tile>> = input.clone();
    let mut b: Vec<Vec<Tile>> = input;

    loop {
        for row in 0..a.len() {
            for col in 0..(a[0].len()) {
                let adjacent = get_around(row as i32, col as i32, &a);
                let this = a[row][col];
                match this {
                    Tile::EmptySeat => {
                        if adjacent == 0 {
                            b[row][col] = Tile::OccupiedSeat;
                        }
                    },
                    Tile::OccupiedSeat => {
                        if adjacent >= 4 {
                            b[row][col] = Tile::EmptySeat;
                        }
                    },
                    _ => continue,
                }
            }
        }

        if a == b {
            break;
        }
        a = b.clone();
    }

    a.iter()
        .map(|row| row.iter().filter(|t| **t == Tile::OccupiedSeat).count())
        .sum()
}

fn get_around(row: i32, col: i32, m: &[Vec<Tile>]) -> usize {
    let potentials = [(row - 1, col - 1), (row, col - 1), (row + 1, col - 1), (row + 1, col), (row + 1, col + 1), (row, col + 1), (row - 1, col + 1), (row - 1, col)];
    let max_row = (m.len() - 1) as i32;
    let max_col = (m[0].len() - 1) as i32;

    let potentials: Vec<(i32, i32)> = potentials.iter()
        .filter(|(row, col)| *row >= 0 && *row <= max_row && *col >= 0 && * col <= max_col)
        .copied()
        .collect();

    let tiles = potentials.iter()
        .map(|(row, col)| m[(*row) as usize][(*col) as usize])
        .filter(|tile| *tile == Tile::OccupiedSeat)
        .count();

    tiles
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 37;
        let actual = solve(input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 2126;
        let actual = solve(input);

        assert_eq!(actual, expected)
    }
    //
    // #[test]
    // fn test_example_part2() {
    //     let input = include_str!("example");
    //     let input = parse_input(input);
    //
    //     let expected = 0;
    //     let actual = solve2(&input);
    //
    //     assert_eq!(actual, expected)
    // }
    //
    // #[test]
    // fn test_solution_part2() {
    //     let input = include_str!("input");
    //     let input = parse_input(input);
    //
    //     let expected: usize = 0;
    //     let actual = solve2(&input);
    //
    //     assert_eq!(actual, expected)
    // }

}