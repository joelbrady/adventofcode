use std::collections::HashSet;

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
    #[allow(dead_code)]
    fn union(&self, other: &State) -> State {
        let a = self.0.union(&other.0);

        State(a.cloned().collect())
    }

    #[allow(dead_code)]
    fn display(&self, z: i32) {
        let (min_x, _, _) = self.0.iter().min_by_key(|(x, _, _)| *x).unwrap();
        let (max_x, _, _) = self.0.iter().max_by_key(|(x, _, _)| *x).unwrap();
        let (_, min_y, _) = self.0.iter().min_by_key(|(_, y, _)| *y).unwrap();
        let (_, max_y, _) = self.0.iter().max_by_key(|(_, y, _)| *y).unwrap();

        dbg!(&min_x, &max_x, &min_y, &max_y, z);

        for y in *min_y..=*max_y {
            for x in *min_x..=*max_x {
                if self.0.contains(&(x, y, z)) {
                    print!("#")
                } else {
                    print!(".")
                }
            }
            println!();
        }
        println!();
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
    // find all 9 cubes around every currently active cube, then union the set
    // these are the only ones that could possibly be active in the next cycle
    let neighbours: HashSet<(i32, i32, i32)> = state.0.iter()
        .flat_map(|t| around(t))
        .collect();

    let mut new = HashSet::with_capacity(neighbours.len() * 2);

    for n in neighbours {
        let ns = around(&n).filter(|position| state.0.contains(position)).count();
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

    for dx in -1..2 {
        for dy in -1..2 {
            for dz in -1..2 {
                if dx == 0 && dy == 0 && dz == 0 {
                    continue
                }
                let d = (dx, dy, dz);
                let a = add(t, &d);
                items.push(a);
            }
        }
    }

    debug_assert_eq!(items.len(), 26);

    items.into_iter()
}

fn add(a: &(i32, i32, i32), b: &(i32, i32, i32)) -> (i32, i32, i32) {
    let (xa, ya, za) = *a;
    let (xb, yb, zb) = *b;

    (xa + xb, ya + yb, za + zb)
}

fn solve(input: &Input) -> usize {
    let mut state = input.initial_state.clone();
    for _ in 0..6 {
        state = cycle(&state);
    }

    state.0.len()
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
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 112;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 237;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 0;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
