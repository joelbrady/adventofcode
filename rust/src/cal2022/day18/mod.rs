use std::collections::{HashSet, VecDeque};

use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::IResult;
use nom::multi::separated_list1;

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
    cubes: Vec<(i64, i64, i64)>,
}

fn parse_input(s: &str) -> Input {
    let (_, cubes) = separated_list1(line_ending, parse_cube)(s).unwrap();

    Input { cubes }
}

fn parse_cube(s: &str) -> IResult<&str, (i64, i64, i64)> {
    let (rem, a) = nom::character::complete::i64(s)?;
    let (rem, _) = tag(",")(rem)?;
    let (rem, b) = nom::character::complete::i64(rem)?;
    let (rem, _) = tag(",")(rem)?;
    let (rem, c) = nom::character::complete::i64(rem)?;

    Ok((rem, (a, b, c)))
}

fn solve_part1(input: &Input) -> i64 {
    let cubes: HashSet<_> = input.cubes.iter().copied().collect();
    cubes.iter()
        .map(|(x, y, z)| {
            let sides_covered = dirs().iter()
                .map(|(dx, dy, dz)| (x + dx, y + dy, z + dz))
                .map(|(x, y, z)| i64::from(cubes.contains(&(x, y, z))))
                .sum::<i64>();
            6 - sides_covered
        })
        .sum()
}

fn dirs() -> &'static [(i64, i64, i64)] {
    &[
        (-1, 0, 0),
        (1, 0, 0),
        (0, -1, 0),
        (0, 1, 0),
        (0, 0, -1),
        (0, 0, 1),
    ]
}

fn solve_part2(input: &Input) -> i64 {
    let outside: HashSet<(i64, i64, i64)> = flood_outside(&input.cubes);

    input.cubes.iter()
        .map(|(x, y, z)| {
            let sides_touching_outside = dirs().iter()
                .map(|(dx, dy, dz)| (x + dx, y + dy, z + dz))
                .filter(|(x, y, z)| outside.contains(&(*x, *y, *z)))
                .count() as i64;
            sides_touching_outside
        })
        .sum()
}

fn flood_outside(cubes: &[(i64, i64, i64)]) -> HashSet<(i64, i64, i64)> {
    let (min , max) = bounding_box(cubes);
    let cubes: HashSet<(i64, i64, i64)> = cubes.iter().copied().collect();
    let (minx, miny, minz) = add(min, (-1, -1, -1));
    let (maxx, maxy, maxz) = add(max, (1, 1, 1));

    let mut outside = HashSet::new();

    let mut queue = VecDeque::new();
    queue.push_back((minx, miny, minz));

    while let Some(current) = queue.pop_front() {
        if outside.contains(&current) || cubes.contains(&current) {
            continue;
        }

        let (x, y, z) = current;
        if x < minx || y < miny || z < minz || x > maxx || y > maxy || z > maxz {
            continue;
        }

        let children = dirs().iter()
            .map(|(dx, dy, dz)| (x + dx, y + dy, z + dz));

        queue.extend(children);
        outside.insert(current);
    }

    outside
}

fn add((ax, ay, az): (i64, i64, i64), (bx, by, bz): (i64, i64, i64)) -> (i64, i64, i64) {
    (ax + bx, ay + by, az + bz)
}

fn bounding_box(cubes: &[(i64, i64, i64)]) -> ((i64, i64, i64), (i64, i64, i64)) {
    let mut minx = i64::MAX;
    let mut miny = i64::MAX;
    let mut minz = i64::MAX;
    
    let mut maxx = i64::MIN;
    let mut maxy = i64::MIN;
    let mut maxz = i64::MIN;
    
    for (x, y, z) in cubes {
        minx = minx.min(*x);
        miny = miny.min(*y);
        minz = minz.min(*z);

        maxx = maxx.max(*x);
        maxy = maxy.max(*y);
        maxz = maxz.max(*z);
    }

    ((minx, miny, minz), (maxx, maxy, maxz))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 64;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 4242;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 58;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 2428;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}