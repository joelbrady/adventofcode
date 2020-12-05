pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Map {
    max_row: usize,
    max_col: usize,
    grid: Vec<Vec<Tile>>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Tile {
    Open,
    Tree,
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            '.' => Tile::Open,
            '#' => Tile::Tree,
            _ => unimplemented!(),
        }
    }
}

fn parse_input(input: &str) -> Map {
    let grid: Vec<Vec<Tile>> = input.lines()
        .map(|line| parse_line(line))
        .collect();

    let max_row = (&grid).len();
    let max_col = grid[0].len();

    Map {
        grid,
        max_col,
        max_row,
    }
}

fn parse_line(line: &str) -> Vec<Tile> {
    line.chars()
        .map(Tile::from)
        .collect()
}

#[derive(Debug)]
struct Vector {
    right: usize,
    down: usize,
}

fn solve(input: &Map) -> usize {
    let vector = Vector { right: 3, down: 1};
    let tiles_encountered = walk(input, &vector);

    trees_in_walk(&tiles_encountered)
}

fn trees_in_walk(walk: &[Tile]) -> usize {
    walk.iter()
        .filter(|tile| **tile == Tile::Tree)
        .count()
}

fn walk(input: &Map, vector: &Vector) -> Vec<Tile> {
    let mut row = 0;
    let mut col = 0;
    let mut seen = vec![];
    while row < input.max_row {
        let tile = input.grid[row][col];
        seen.push(tile);
        row += vector.down;
        col = (col + vector.right) % input.max_col;
    }

    seen
}

fn solve2(input: &Map) -> usize {
    let vs = vec![
        Vector { right: 1, down: 1},
        Vector { right: 3, down: 1},
        Vector { right: 5, down: 1},
        Vector { right: 7, down: 1},
        Vector { right: 1, down: 2},
    ];

    vs.iter()
        .map(|v| walk(input, v))
        .map(|walk| trees_in_walk(&walk))
        .product()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 7;
        let actual = solve(&input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 162;
        let actual = solve(&input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 336;
        let actual = solve2(&input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 3064612320;
        let actual = solve2(&input);

        assert_eq!(expected, actual);
    }
}