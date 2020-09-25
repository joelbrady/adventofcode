use std::cmp::Ordering::{Equal, Greater, Less};
use std::collections::HashSet;

pub fn main() {
    let input = include_str!("input");
    let (x, y) = solve(input);

    println!("The first collision is at {},{}", x, y);
}

struct Tracks {
    tracks: Vec<Vec<Track>>,
    carts: Vec<Cart>,
}

impl Tracks {
    fn from_str(input: &str) -> Tracks {
        let mut arrays = get_input_arrays(input);
        let carts = find_carts_and_fill_track(&mut arrays);
        let tracks: Vec<Vec<Track>> = parse_tracks(&arrays);
        Tracks { tracks, carts }
    }

    fn display(&self) {
        let rows = self.tracks.len();
        let cols = self.tracks[0].len();

        for y in 0..rows {
            for x in 0..cols {
                let cart: Option<char> = self.cart_at(x, y);
                match cart {
                    Some(cart) => print!("{}", cart),
                    None => {
                        let track = track_to_char(&self.tracks[y][x]);
                        print!("{}", track);
                    }
                }
            }
            println!();
        }
    }

    fn cart_at(&self, x: usize, y: usize) -> Option<char> {
        let cs: Vec<&Cart> = self.carts.iter()
            .filter(|cart| cart.position == (x, y))
            .collect();

        if cs.len() == 1 {
            Some(cs[0].as_char())
        } else if cs.is_empty() {
            None
        } else {
            Some('X')
        }
    }

    fn step(&mut self) {
        use Direction::*;

        let mut sorted_carts: Vec<Cart> = self.carts.to_vec();
        sorted_carts.sort_by(|a, b| {
            let (ax, ay) = a.position;
            let (bx, by) = b.position;
            if ay > by {
                Greater
            } else if ay < by {
                Less
            } else {
                if ax > bx {
                    Greater
                } else if ax < bx {
                    Less
                } else {
                    Equal
                }
            }
        });

        for i in 0..sorted_carts.len() {
            let cart = &sorted_carts[i];
            let (x, y) = cart.position;
            let (x, y) = match cart.direction {
                North => (x, y - 1),
                South => (x, y + 1),
                East => (x + 1, y),
                West => (x - 1, y),
            };
            let (direction, choice) = new_direction(&self.tracks[y][x], &cart.direction, cart.choice);
            let cart = Cart{ position: (x, y), direction, choice };
            sorted_carts[i] = cart;
            if find_collision(&sorted_carts).is_some() {
                break
            }
        }

        self.carts = sorted_carts;
    }

}

fn find_collision(carts: &[Cart]) -> Option<(usize, usize)> {
    let mut seen: HashSet<&(usize, usize)> = HashSet::new();
    for cart in carts {
        let pos = cart.position;
        if seen.contains(&pos) {
            return Some(pos)
        } else {
            seen.insert(&cart.position);
            continue
        }
    }

    None
}

fn new_direction(track: &Track, direction: &Direction, choice: i32) -> (Direction, i32) {
    use Direction::*;
    use Track::*;

    match track {
        LeftCurve => (match direction {
            North => West,
            South => East,
            East => South,
            West => North,
        }, choice),
        RightCurve => (match direction {
            North => East,
            East => North,
            South => West,
            West => South,
        }, choice),
        Intersection => {
            let choice = choice % 3;
            (match choice {
                0 => turn_left(&direction),
                1 => *direction,
                2 => turn_right(&direction),
                _ => unimplemented!(),
            }, choice + 1)
        },
        _ => (*direction, choice),
    }
}

fn turn_left(direction: &Direction) -> Direction {
    use Direction::*;

    match direction {
        North => West,
        West => South,
        South => East,
        East => North,
    }
}

fn turn_right(direction: &Direction) -> Direction {
    use Direction::*;

    match direction {
        North => East,
        East => South,
        South => West,
        West => North,
    }
}

#[derive(Clone, Copy)]
struct Cart {
    position: (usize, usize),
    direction: Direction,
    choice: i32,
}

impl Cart {
    fn as_char(&self) -> char {
        use Direction::*;

        match self.direction {
            North => '^',
            South => 'v',
            East => '>',
            West => '<',
        }
    }
}

#[derive(Copy, Clone)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Debug, Eq, PartialEq)]
enum Track {
    HorizontalStraight,
    // -
    VerticalStraight,
    // |
    LeftCurve,
    // \
    RightCurve,
    // /
    Intersection,
    // +
    Empty,
}

fn solve(input: &str) -> (usize, usize) {
    let mut tracks = Tracks::from_str(input);

    loop {
        let collision = find_collision(&tracks.carts);
        match collision {
            Some(pos) => {
                tracks.display();
                return pos
            },
            None => {
                tracks.step();
            }
        }
    }
}

fn get_input_arrays(input: &str) -> Vec<Vec<char>> {
    let input = input.trim_end_matches('\n');
    let lines: Vec<Vec<char>> = input.split('\n')
        .map(|s| s.chars().collect())
        .collect();
    let length = lines[0].len();
    lines.iter()
        .for_each(|line| assert_eq!(line.len(), length));
    lines
}

fn parse_tracks(input: &Vec<Vec<char>>) -> Vec<Vec<Track>> {
    input.iter()
        .map(|row| row.iter()
            .map(char_to_track)
            .collect())
        .collect()
}

fn char_to_track(c: &char) -> Track {
    use Track::*;

    match c {
        '|' => VerticalStraight,
        '-' => HorizontalStraight,
        '+' => Intersection,
        '/' => RightCurve,
        '\\' => LeftCurve,
        _ => Empty
    }
}

fn track_to_char(track: &Track) -> char {
    use Track::*;

    match track {
        VerticalStraight => '|',
        HorizontalStraight => '-',
        Intersection => '+',
        RightCurve => '/',
        LeftCurve => '\\',
        Empty => ' ',
    }
}

fn track_under_cart(cart: char) -> char {
    if cart == '>' || cart == '<' {
        '-'
    } else {
        '|'
    }
}

fn find_carts_and_fill_track(tracks: &mut Vec<Vec<char>>) -> Vec<Cart> {
    use Direction::*;

    let rows = tracks.len();
    let cols = tracks[0].len();
    let mut carts: Vec<Cart> = Vec::new();

    for y in 0..rows {
        for x in 0..cols {
            let tile = tracks[y][x];
            let direction: Option<Direction> = match tile {
                '>' => Some(East),
                '<' => Some(West),
                '^' => Some(North),
                'v' => Some(South),
                _ => None,
            };
            match direction {
                Some(direction) => {
                    let cart = Cart { direction, position: (x, y), choice: 0 };
                    carts.push(cart); // push little cart!
                    tracks[y][x] = track_under_cart(tile);
                }
                _ => continue,
            };
        }
    }

    carts
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_example() {
        assert_eq!(solve(include_str!("example.input")), (7, 3))
    }

    #[test]
    fn test_get_input_arrays() {
        let arrays = get_input_arrays(include_str!("example.input"));
        assert_eq!(arrays[0][0], '/');
        assert_eq!(arrays[0][1], '-');
        assert_eq!(arrays[0][2], '>');
        assert_eq!(arrays[1][0], '|');
    }

    #[test]
    fn test_parse_tracks() {
        use Track::*;

        let input = vec![vec!['\\', '/', '/'], vec!['-', '+', '|']];
        assert_eq!(parse_tracks(&input)[..], vec![
            vec![LeftCurve, RightCurve, RightCurve],
            vec![HorizontalStraight, Intersection, VerticalStraight]
        ][..]);
    }

    #[test]
    fn solve_puzzle() {
        main()
    }
}
