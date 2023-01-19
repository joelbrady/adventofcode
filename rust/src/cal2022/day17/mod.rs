use std::collections::{HashMap, HashSet};

use nom::{IResult, Parser};
use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::multi::many1;
use nom::sequence::terminated;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    moves: Vec<Move>,
}

#[derive(Debug, Clone, Copy)]
enum Move {
    Left,
    Right,
}

fn parse_input(s: &str) -> Input {
    let msp = many1(parse_move);
    let moves = terminated(msp, line_ending)(s).unwrap().1;

    Input { moves }
}

fn parse_move(s: &str) -> IResult<&str, Move> {
    let lp = tag("<").map(|_| Move::Left);
    let rp = tag(">").map(|_| Move::Right);
    let mut mp = lp.or(rp);

    mp.parse(s)
}

const CHAMBER_WIDTH: usize = 7;
const FLOOR_Y: i64 = 0;

#[derive(Debug)]
struct Chamber {
    moving_piece: Piece,
    resting_piece_blocks: HashSet<(i64, i64)>,
    stopped_rocks: i64,
    moves: Vec<Move>,
    current_move: usize,
    shapes: Vec<PieceShape>,
    next_shape: usize,
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct ViewFromTheTop([i64; 7]);

impl Chamber {
    fn new(starting_piece: Piece, moves: &[Move]) -> Self {
        use PieceShape::*;

        Self {
            moving_piece: starting_piece,
            resting_piece_blocks: HashSet::new(),
            stopped_rocks: 0,
            moves: moves.to_vec(),
            current_move: 0,
            shapes: vec![HLine, Cross, Ell, VLine, Square],
            next_shape: 1,
        }
    }

    #[allow(dead_code)]
    fn print(&self) {
        let highest_y = self.moving_piece.blocks.iter()
            .map(|(_x, y)| y)
            .max()
            .unwrap();

        for y in (0..(highest_y + 2)).rev() {
            for x in 0..9 {
                if y == 0 {
                    if x == 0 || x == 8 {
                        print!("+");
                    } else {
                        print!("-");
                    }
                } else if x == 0 || x == 8 {
                    print!("|");
                } else if self.resting_piece_blocks.contains(&(x, y)) {
                    print!("#");
                } else if self.moving_piece.blocks.contains(&(x, y)) {
                    print!("@");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }

    fn step(&mut self) -> Option<(usize, PieceShape, ViewFromTheTop)> {
        // first try the jet move
        let m = self.moves[self.current_move % self.moves.len()];
        self.current_move += 1;
        let (dx, dy) = match m {
            Move::Left => (-1, 0),
            Move::Right => (1, 0),
        };

        let new_piece_coords: HashSet<(i64, i64)> = self.moving_piece.blocks.iter()
            .map(|(x, y)| (x + dx, y + dy))
            .collect();

        let collides = new_piece_coords.iter()
            .copied()
            .any(|(x, y)| x == 0
                || x == (CHAMBER_WIDTH as i64 + 1)
                || y == FLOOR_Y
                || self.resting_piece_blocks.contains(&(x, y)));

        if !collides {
            self.moving_piece = Piece { blocks: new_piece_coords };
        }

        // then try moving down
        let new_piece_coords: HashSet<(i64, i64)> = self.moving_piece.blocks.iter()
            .map(|(x, y)| (*x, y - 1))
            .collect();

        let collides = new_piece_coords.iter()
            .copied()
            .any(|(x, y)| x == 0
                || x == (CHAMBER_WIDTH as i64 + 1)
                || y == FLOOR_Y
                || self.resting_piece_blocks.contains(&(x, y)));

        if !collides {
            self.moving_piece = Piece { blocks: new_piece_coords };
            None
        } else {
            let new_shape = self.shapes[self.next_shape % self.shapes.len()];
            self.next_shape += 1;

            let highest_y = *self.moving_piece.blocks.iter().chain(self.resting_piece_blocks.iter())
                .map(|(_x, y)| y)
                .max()
                .unwrap();

            let new_piece: Piece = Piece::new(new_shape, highest_y);
            let old_piece = std::mem::replace(&mut self.moving_piece, new_piece);
            self.resting_piece_blocks.extend(old_piece.blocks);
            self.stopped_rocks += 1;
            let view = self.heights_for_all_columns();
            Some((self.current_move % self.moves.len(), new_shape, view))
        }
    }

    fn heights_for_all_columns(&self) -> ViewFromTheTop {
        let mut a = [0; CHAMBER_WIDTH];

        #[allow(clippy::needless_range_loop)]
        for i in 0..CHAMBER_WIDTH {
            a[i] = self.resting_piece_blocks.iter()
                .filter(|(x, _y)| ((i as i64) + 1) == *x)
                .map(|(_x, y)| y)
                .copied()
                .max()
                .unwrap_or(0);
        }

        let min_y = *a.iter()
            .min()
            .unwrap();

        let a = a.map(|n| n - min_y);

        ViewFromTheTop(a)
    }

    fn height(&self) -> i64 {
        self.resting_piece_blocks.iter()
            .map(|(_x, y)| y)
            .copied()
            .max()
            .unwrap_or(0)
    }
}

#[derive(Debug)]
struct Piece {
    blocks: HashSet<(i64, i64)>,
}

impl Piece {
    fn new(shape: PieceShape, highest_y: i64) -> Self {
        let blocks = match shape {
            PieceShape::HLine => {
                [3, 4, 5, 6].into_iter()
                    .map(|x| (x, highest_y + 4))
                    .collect()
            }
            PieceShape::Cross => {
                [
                    (4, highest_y + 6),
                    (4, highest_y + 5),
                    (4, highest_y + 4),
                    (3, highest_y + 5),
                    (5, highest_y + 5),
                ]
                    .into_iter()
                    .collect()
            }
            PieceShape::Ell => {
                [
                    (5, highest_y + 6),
                    (5, highest_y + 5),
                    (5, highest_y + 4),
                    (4, highest_y + 4),
                    (3, highest_y + 4),
                ]
                    .into_iter()
                    .collect()
            }
            PieceShape::VLine => {
                (0..4)
                    .map(|dy| (3, highest_y + dy + 4))
                    .collect()
            }
            PieceShape::Square => {
                [
                    (3, highest_y + 5),
                    (4, highest_y + 5),
                    (3, highest_y + 4),
                    (4, highest_y + 4),
                ]
                    .into_iter()
                    .collect()
            }
        };

        Self { blocks }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum PieceShape {
    HLine,
    Cross,
    Ell,
    VLine,
    Square,
}

fn solve_part1(input: &Input) -> i64 {
    let starting_piece = Piece::new(PieceShape::HLine, 0);
    let mut chamber = Chamber::new(starting_piece, &input.moves);

    while chamber.stopped_rocks < 2022 {
        chamber.step();
    }

    *chamber.resting_piece_blocks.iter()
        .map(|(_x, y)| y)
        .max()
        .unwrap()
}

fn solve_part2(input: &Input) -> i64 {
    let starting_piece = Piece::new(PieceShape::HLine, 0);
    let mut chamber = Chamber::new(starting_piece, &input.moves);

    let mut m: HashMap<(usize, PieceShape, ViewFromTheTop), (i64, i64)> = HashMap::new();

    loop {
        if let Some(view) = chamber.step() {
            #[allow(clippy::map_entry)]
            if m.contains_key(&view) {
                let height_at_end_of_cycle = chamber.height();
                let (rocks_at_start_of_cycle, height_at_start_of_cycle) = m.get(&view).unwrap();
                let cycle_length = chamber.stopped_rocks - rocks_at_start_of_cycle;
                let height_delta = height_at_end_of_cycle - height_at_start_of_cycle;

                let target = 1000000000000;
                let target = target - rocks_at_start_of_cycle;

                let cycle_count = target / cycle_length;
                let remainder = target % cycle_length;

                let height_before_remainder = chamber.height();

                let remainder_target = chamber.stopped_rocks + remainder;

                while chamber.stopped_rocks < remainder_target {
                    chamber.step();
                }

                let final_height = chamber.height();
                let height_delta_for_remainder = final_height - height_before_remainder;

                return (cycle_count * height_delta) + height_delta_for_remainder + height_at_start_of_cycle;
            } else {
                m.insert(view, (chamber.stopped_rocks, chamber.height()));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 3068;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 3171;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 1514285714288;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 1586627906921;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}