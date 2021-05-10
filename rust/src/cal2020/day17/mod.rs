use std::collections::HashSet;
use std::ops::Range;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(&input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Clone)]
struct Input {
    initial_state: State,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct State(HashSet<(i32, i32, i32)>);

impl State {
    fn union(&self, other: &State) -> State {
        let a = self.0.union(&other.0);

        State(a.cloned().collect())
    }
}

fn parse_input(input: &str) -> Input {
    let initial_state = parse_input_with_z(input, 0);

    Input { initial_state }
}

fn parse_input_with_z(input: &str, z: i32) -> State {
    let grid: Vec<Vec<char>> = input.lines()
        .map(|line| line.chars().collect())
        .collect();

    let mut set = HashSet::new();

    for y in 0..(grid.len()) {
        for x in 0..(grid[0].len()) {
            if grid[y][x] == '#' {
                let p = (x as i32, y as i32, z);
                set.insert(p);
            }
        }
    }

    State(set)
}

fn cycle(state: &State) -> State {
    dbg!(state);
    // find all 9 cubes around every currently active cube, then union the set
    // these are the only ones that could possibly be active in the next cycle
    let neighbours: HashSet<(i32, i32, i32)> = state.0.iter()
        .flat_map(|t| around(t))
        .collect();

    let mut new = HashSet::with_capacity(neighbours.len() * 2);

    for n in neighbours {
        let ns = around(&n).filter(|position| state.0.contains(position)).count();
        dbg!(&n, &ns);
        if state.0.contains(&n) {
            if (2..=3).contains(&ns) {
                new.insert(n);
            }
        } else if ns == 3 {
            new.insert(n);
        }
    }

    State(new)
}

fn around(t: &(i32, i32, i32)) -> impl Iterator<Item=(i32, i32, i32)> {

    let mut items = Vec::with_capacity(3 * 3 * 3);

    for d in g() {
        let a = add(t, &d);
        items.push(a);
    }

    items.into_iter()
}

fn add(a: &(i32, i32, i32), b: &(i32, i32, i32)) -> (i32, i32, i32) {
    let (xa, ya, za) = *a;
    let (xb, yb, zb) = *b;

    (xa + xb, ya + yb, za + zb)
}

fn g() -> impl Iterator<Item=(i32, i32, i32)> {
    f().zip(f()).zip(f()).map(|((x, y), z)| (x, y, z))
}

fn f() -> Range<i32> {
    -1..2
}

fn solve(_input: &Input) -> usize {
    todo!()
}

fn solve2(_input: &Input) -> usize {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_example() {
        let input = include_str!("example");
        let actual = parse_input(input);

        let mut expected = HashSet::new();

        // .#.
        // ..#
        // ###

        expected.insert((1, 0, 0));
        expected.insert((2, 1, 0));
        expected.insert((0, 2, 0));
        expected.insert((1, 2, 0));
        expected.insert((2, 2, 0));

        assert_eq!(actual.initial_state, State(expected))
    }

    #[test]
    fn test_example_first_cycle() {
        //Before any cycles:
        //
        // z=0
        // .#.
        // ..#
        // ###

        let start = parse_input_with_z(".#.\n..#\n###", 0);

        // After 1 cycle:
        //
        // z=-1
        // #..
        // ..#
        // .#.
        //
        // z=0
        // #.#
        // .##
        // .#.
        //
        // z=1
        // #..
        // ..#
        // .#.

        let expected_z_minus_1 = parse_input_with_z("#..\n..#\n.#.", -1);
        let expected_z_0 = parse_input_with_z("#.#\n.##\n.#.", 0);
        let expected_z_plus_1 = parse_input_with_z("#..\n..#\n.#.", 1);

        let expected_first_cycle = expected_z_minus_1.union(&expected_z_0).union(&expected_z_plus_1);

        let first_cycle = cycle(&start);

        assert_eq!(first_cycle, expected_first_cycle)
    }

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 71;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 25972;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 622670335901;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
