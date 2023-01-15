use itertools::{Itertools, MinMaxResult};
use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::IResult;
use nom::multi::separated_list1;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(&input, 2000000);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input, (4000000, 4000000));
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    sensors: Vec<Sensor>,
}

#[derive(Debug)]
struct Sensor {
    position: (i64, i64),
    closest_beacon: (i64, i64),
    beacon_distance: i64,
}

impl Sensor {
    fn new(position: (i64, i64), closest_beacon: (i64, i64)) -> Self {
        let beacon_distance = manhattan_distance(position, closest_beacon);

        Self {
            position,
            closest_beacon,
            beacon_distance,
        }
    }

    fn distance_from_sensor(&self, p: (i64, i64)) -> i64 {
        manhattan_distance(self.position, p)
    }

    fn is_in_range(&self, p: (i64, i64)) -> bool {
        self.distance_from_sensor(p) <= self.beacon_distance && p != self.position && p != self.closest_beacon
    }

    fn border_points(&self) -> Vec<(i64, i64)> {
        let d = self.beacon_distance;
        let (x, y) = self.position;
        let left = (x - d - 1, y);
        let right = (x + d + 1, y);

        let mut result = vec![];

        let (mut x, mut y) = left;
        for _ in 0..=(d + 1) {
            result.push((x, y));
            x += 1;
            y -= 1;
        }

        let (mut x, mut y) = left;
        for _ in 0..=(d + 1) {
            result.push((x, y));
            x += 1;
            y += 1;
        }

        let (mut x, mut y) = right;
        for _ in 0..=(d + 1) {
            result.push((x, y));
            x -= 1;
            y -= 1;
        }

        let (mut x, mut y) = right;
        for _ in 0..=(d + 1) {
            result.push((x, y));
            x -= 1;
            y += 1;
        }

        for p in &result {
            assert_eq!(self.distance_from_sensor(*p), self.beacon_distance + 1)
        }

        result
    }
}

fn manhattan_distance((ax, ay): (i64, i64), (bx, by): (i64, i64)) -> i64 {
    (ax - bx).abs() + (ay - by).abs()
}

fn parse_input(s: &str) -> Input {
    let (_, sensors) = separated_list1(line_ending, parse_sensor)(s).unwrap();
    Input { sensors }
}

fn parse_sensor(s: &str) -> IResult<&str, Sensor> {
    let int = nom::character::complete::i64;

    let (rem, _) = tag("Sensor at x=")(s)?;
    let (rem, x) = int(rem)?;
    let (rem, _) = tag(", y=")(rem)?;
    let (rem, y) = int(rem)?;
    let position = (x, y);
    let (rem, _) = tag(": closest beacon is at x=")(rem)?;
    let (rem, x) = int(rem)?;
    let (rem, _) = tag(", y=")(rem)?;
    let (rem, y) = int(rem)?;
    let closest_beacon = (x, y);

    Ok((rem, Sensor::new(position, closest_beacon)))
}

fn solve_part1(input: &Input, y: i64) -> i64 {
    let (min_x, max_x) = match input.sensors.iter()
        .flat_map(|s| {
            let d = s.beacon_distance;
            [s.position.0 + d, s.position.0 - d, s.closest_beacon.0 - d, s.closest_beacon.0 + d].into_iter()
        })
        .minmax() {
        MinMaxResult::NoElements => panic!(),
        MinMaxResult::OneElement(n) => (n, n),
        MinMaxResult::MinMax(min, max) => (min, max),
    };

    (min_x..=max_x).into_iter()
        .filter(|x| {
            let p = (*x, y);
            input.sensors.iter().any(|s| s.is_in_range(p))
        })
        .count() as i64
}

fn solve_part2(input: &Input, (max_x, max_y): (i64, i64)) -> i64 {
    for sensor in &input.sensors {
        let bps = sensor.border_points();
        for p in &bps {
            let (x, y) = p;
            if *x < 0 || *y < 0 || *x > max_x || *y > max_y {
                continue;
            }
            if input.sensors.iter()
                .filter(|s| s.position != sensor.position)
                .all(|s| {
                    let d = s.distance_from_sensor(*p);
                    d > s.beacon_distance
                }) {
                return (x * 4000000) + y
            }
        }

    }

    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 26;
        let actual = solve_part1(&input, 10);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 5688618;
        let actual = solve_part1(&input, 2000000);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 56000011;
        let actual = solve_part2(&input, (20, 20));

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 12625383204261;
        let actual = solve_part2(&input, (4000000, 4000000));

        assert_eq!(actual, expected)
    }
}