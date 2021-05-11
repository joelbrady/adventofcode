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
    initial_state: HashSet<(i32, i32)>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct State(HashSet<(i32, i32, i32)>);

#[derive(Debug, Clone, Eq, PartialEq)]
struct State2(HashSet<(i32, i32, i32, i32)>);

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
    let grid: Vec<Vec<char>> = input.lines()
        .map(|line| line.chars().collect())
        .collect();

    let mut initial_state = HashSet::new();

    for y in 0..(grid.len()) {
        for x in 0..(grid[0].len()) {
            if grid[y][x] == '#' {
                let p = (x as i32, y as i32);
                initial_state.insert(p);
            }
        }
    }

    Input { initial_state }
}

fn cycle(state: &State) -> State {
    // find all cubes around every currently active cube, then union the set
    // these are the only ones that could possibly be active in the next cycle
    let neighbours: HashSet<(i32, i32, i32)> = state.0.iter()
        .flat_map(|t| around3(t))
        .collect();

    let mut new = HashSet::with_capacity(neighbours.len() * 2);

    for n in neighbours {
        let ns = around3(&n).filter(|position| state.0.contains(position)).count();
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

fn cycle2(state: &State2) -> State2 {
    // find all cubes around every currently active cube, then union the set
    // these are the only ones that could possibly be active in the next cycle
    let neighbours: HashSet<(i32, i32, i32, i32)> = state.0.iter()
        .flat_map(|t| around4(t))
        .collect();

    let mut new = HashSet::with_capacity(neighbours.len() * 2);

    for n in neighbours {
        let ns = around4(&n).filter(|position| state.0.contains(position)).count();
        if state.0.contains(&n) {
            if (2..=3).contains(&ns) {
                new.insert(n);
            }
        } else if ns == 3 {
            new.insert(n);
        }
    }

    State2(new)
}

fn around3(t: &(i32, i32, i32)) -> impl Iterator<Item=(i32, i32, i32)> {
    let mut items = Vec::with_capacity(3 * 3 * 3);

    for dx in -1..2 {
        for dy in -1..2 {
            for dz in -1..2 {
                if dx == 0 && dy == 0 && dz == 0 {
                    continue
                }
                let d = (dx, dy, dz);
                let a = add3(t, &d);
                items.push(a);
            }
        }
    }

    debug_assert_eq!(items.len(), (3 * 3 * 3) - 1);

    items.into_iter()
}

fn around4(t: &(i32, i32, i32, i32)) -> impl Iterator<Item=(i32, i32, i32, i32)> {
    let mut items = Vec::with_capacity(3 * 3 * 3);

    for dw in -1..2 {
        for dx in -1..2 {
            for dy in -1..2 {
                for dz in -1..2 {
                    if dw == 0 && dx == 0 && dy == 0 && dz == 0 {
                        continue
                    }
                    let d = (dx, dy, dz, dw);
                    let a = add4(t, &d);
                    items.push(a);
                }
            }
        }
    }

    debug_assert_eq!(items.len(), (3 * 3 * 3 * 3) - 1);

    items.into_iter()
}

fn add3(a: &(i32, i32, i32), b: &(i32, i32, i32)) -> (i32, i32, i32) {
    let (xa, ya, za) = *a;
    let (xb, yb, zb) = *b;

    (xa + xb, ya + yb, za + zb)
}

fn add4(a: &(i32, i32, i32, i32), b: &(i32, i32, i32, i32)) -> (i32, i32, i32, i32) {
    let (xa, ya, za, wa) = *a;
    let (xb, yb, zb, wb) = *b;

    (xa + xb, ya + yb, za + zb, wa + wb)
}

fn solve(input: &Input) -> usize {
    let three_dimensioned = input.initial_state.iter()
        .map(|(x, y)| (*x, *y, 0))
        .collect();

    let mut state = State(three_dimensioned);

    for _ in 0..6 {
        state = cycle(&state);
    }

    state.0.len()
}

fn solve2(input: &Input) -> usize {
    let four_dimensioned = input.initial_state.iter()
        .map(|(x, y)| (*x, *y, 0, 0))
        .collect();

    let mut state = State2(four_dimensioned);

    for _ in 0..6 {
        state = cycle2(&state);
    }

    state.0.len()
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

        expected.insert((1, 0));
        expected.insert((2, 1));
        expected.insert((0, 2));
        expected.insert((1, 2));
        expected.insert((2, 2));

        assert_eq!(actual.initial_state, expected)
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
    fn test_example2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 848;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 2448;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
