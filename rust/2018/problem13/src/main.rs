use std::io::Read;
use std::collections::HashSet;

fn main() {
    let mut track = Tracks::from_str(example_input().as_str());
    track.display();
    track.step();
    track.display();
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
        } else if cs.len() == 0 {
            None
        } else {
            Some('X')
        }
    }

    fn step(&mut self) {
        for i in 0..self.carts.len() {
            let cart = &self.carts[i];
            let (x, y) = cart.position;
            let (x, y) = match cart.direction {
                Direction::North => (x, y - 1),
                Direction::South => (x, y + 1),
                Direction::East => (x + 1, y),
                Direction::West => (x - 1, y),
            };
            let direction = new_direction(&cart.direction, &self.tracks[y][x]);
            let cart = Cart { position: (x, y), direction };
            self.carts[i] = cart;
        }
    }

    fn has_collision(&self) -> bool {
        let mut seen: HashSet<&(usize, usize)> = HashSet::new();
        for cart in &self.carts {
            let pos = cart.position;
            if seen.contains(&pos) {
                return true
            } else {
                seen.insert(&cart.position);
                continue
            }
        }

        false
    }
}

fn new_direction(d: &Direction, track: &Track) -> Direction {
    match track {
        Track::LeftCurve => match direction {
            Direction::North => Direction::West
        },
    }
}

struct Cart {
    position: (usize, usize),
    direction: Direction,
}

impl Cart {
    fn as_char(&self) -> char {
        match self.direction {
            Direction::North => '^',
            Direction::South => 'v',
            Direction::East => '>',
            Direction::West => '<',
        }
    }
}

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

// given a
fn solve(input: &str) -> (i32, i32) {
    unimplemented!()
}

fn get_input_arrays(input: &str) -> Vec<Vec<char>> {
    let input = input.trim_end_matches("\n");
    let lines: Vec<Vec<char>> = input.split("\n")
        .map(|s| s.chars().collect())
        .collect();
    let length = lines[0].len();
    lines.iter()
        .for_each(|line| assert_eq!(line.len(), length));
    lines
}

fn example_input() -> String {
    let mut file = std::fs::File::open("./example.input").unwrap();
    let mut input = String::new();
    file.read_to_string(&mut input).expect("problem reading file");
    input
}

fn parse_tracks(input: &Vec<Vec<char>>) -> Vec<Vec<Track>> {
    input.iter()
        .map(|row| row.iter()
            .map(char_to_track)
            .collect())
        .collect()
}

fn char_to_track(c: &char) -> Track {
    match c {
        '|' => Track::VerticalStraight,
        '-' => Track::HorizontalStraight,
        '+' => Track::Intersection,
        '/' => Track::RightCurve,
        '\\' => Track::LeftCurve,
        _ => Track::Empty
    }
}

fn track_to_char(track: &Track) -> char {
    match track {
        Track::VerticalStraight => '|',
        Track::HorizontalStraight => '-',
        Track::Intersection => '+',
        Track::RightCurve => '/',
        Track::LeftCurve => '\\',
        Track::Empty => ' ',
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
    let rows = tracks.len();
    let cols = tracks[0].len();
    let mut carts: Vec<Cart> = Vec::new();

    for y in 0..rows {
        for x in 0..cols {
            let tile = tracks[y][x];
            let direction: Option<Direction> = match tile {
                '>' => Some(Direction::East),
                '<' => Some(Direction::West),
                '^' => Some(Direction::North),
                'v' => Some(Direction::South),
                _ => None,
            };
            match direction {
                Some(direction) => {
                    let cart = Cart { direction, position: (x, y) };
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
        assert_eq!(solve(example_input().as_str()), (7, 3))
    }

    #[test]
    fn test_get_input_arrays() {
        let arrays = get_input_arrays(example_input().as_str());
        assert_eq!(arrays[0][0], '/');
        assert_eq!(arrays[0][1], '-');
        assert_eq!(arrays[0][2], '>');
        assert_eq!(arrays[1][0], '|');
    }

    #[test]
    fn test_parse_tracks() {
        let input = vec![vec!['\\', '/', '/'], vec!['-', '+', '|']];
        assert_eq!(parse_tracks(&input)[..], vec![
            vec![Track::LeftCurve, Track::RightCurve, Track::RightCurve],
            vec![Track::HorizontalStraight, Track::Intersection, Track::VerticalStraight]
        ][..]);
    }
}
