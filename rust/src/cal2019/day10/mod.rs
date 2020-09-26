use std::collections::HashSet;
use std::f64::consts::PI;

use num_integer::gcd;

pub fn main() {
    let (_, solution) = solve(include_str!("input"));
    println!("the solution to part 1 is {}", solution);
    let solution = solve2(include_str!("input"));
    println!("the solution to part 2 is {}", solution);
}

fn solve(input: &str) -> ((i32, i32), usize) {
    let f = Field::parse(input);
    f.largest_visibility()
}

fn solve2(input: &str) -> i32 {
    let mut f = Field::parse(input);
    let destroyed = f.destroy();
    let (x, y) = destroyed[199];
    (x * 100) + y
}

#[derive(Debug)]
struct Field {
    asteroids: HashSet<(i32, i32)>
}

impl Field {
    fn parse(s: &str) -> Field {
        let mut asteroids: HashSet<(i32, i32)> = HashSet::new();
        let (mut x, mut y) = (0, 0);
        for c in s.chars() {
            match c {
                '.' => x += 1,
                '#' => {
                    asteroids.insert((x, y));
                    x += 1;
                }
                '\n' => {
                    x = 0;
                    y += 1;
                }
                '\r' => {
                    // ignore on windows
                }
                c => panic!("unexpected char {}", c),
            };
        }
        Field { asteroids }
    }

    fn largest_visibility(&self) -> ((i32, i32), usize) {
        let mut asteroid: &(i32, i32) = &(-1, -1);
        let mut largest = 0;

        for a in self.asteroids.iter() {
            let v = self.visible_from(a);
            let v = v.len();
            if v > largest {
                largest = v;
                asteroid = a;
            }
        }

        (*asteroid, largest)
    }

    fn visible_from(&self, a: &(i32, i32)) -> Vec<(i32, i32)> {
        let mut visible = self.asteroids.clone();
        for b in self.asteroids.iter() {
            if a == b {
                continue;
            }
            let d = sub(b, a);
            let d = simplify(&d);
            let multiples = self.multiples_in_bounds(b, &d);
            if *a == (4, 2) {}
            multiples.iter()
                .for_each(|m| {
                    if visible.remove(m) {}
                });
        }
        visible.remove(a);
        visible.iter()
            .copied()
            .collect()
    }

    fn multiples_in_bounds(&self, x: &(i32, i32), d: &(i32, i32)) -> Vec<(i32, i32)> {
        let mut multiples: Vec<(i32, i32)> = vec![];
        let mut x = add(x, d);
        while self.inside(&x) {
            multiples.push(x);
            x = add(d, &x);
        }
        multiples
    }

    fn inside(&self, pos: &(i32, i32)) -> bool {
        let (x, y) = pos;
        if *x < 0 || *y < 0 {
            false
        } else {
            let max_x = self.asteroids.iter()
                .map(|(ax, _)| ax)
                .max()
                .unwrap();
            let max_y = self.asteroids.iter()
                .map(|(_, ay)| ay)
                .max()
                .unwrap();
            if x > max_x || y > max_y {
                false
            } else {
                true
            }
        }
    }

    fn num_visible_from(&self, a: &(i32, i32)) -> usize {
        self.visible_from(a).len()
    }

    fn destroy(&mut self) -> Vec<(i32, i32)> {
        use std::cmp::Ordering::{Greater, Less, Equal};

        let mut destroyed: Vec<(i32, i32)> = vec![];
        let (station, _) = self.largest_visibility();
        while self.asteroids.len() > 1 {
            let visible = self.visible_from(&station);
            let mut visible: Vec<(f64, (i32, i32))> = visible.iter()
                .map(|v| {
                    let d = sub(v, &station);
                    let (x, y) = d;
                    let d = (x, -y);
                    (angle(&d), *v)
                })
                .collect();
            visible.sort_by(|a, b| {
                let (a, _) = a;
                let (b, _) = b;
                if a > b {
                    Greater
                } else if a < b {
                    Less
                } else {
                    Equal
                }
            });
            visible.iter()
                .for_each(|(_, v)| {
                    destroyed.push(*v);
                    self.asteroids.remove(v);
                });
        }
        destroyed
    }
}

fn add(a: &(i32, i32), b: &(i32, i32)) -> (i32, i32) {
    let (ax, ay) = a;
    let (bx, by) = b;
    (ax + bx, ay + by)
}

fn sub(a: &(i32, i32), b: &(i32, i32)) -> (i32, i32) {
    let (ax, ay) = a;
    let (bx, by) = b;
    (ax - bx, ay - by)
}

fn simplify(d: &(i32, i32)) -> (i32, i32) {
    let (x, y) = *d;
    let g = gcd(x, y);
    (x / g, y / g)
}

fn angle(v: &(i32, i32)) -> f64 {
    let (x, y) = *v;
    if x >= 0 && y > 0 {
        angle_q1(v)
    } else if x > 0 && y < 0 {
        angle_q2(v)
    } else if x < 0 && y < 0 {
        angle_q3(v)
    } else {
        angle_q4(v)
    }
}

fn angle_q1(v: &(i32, i32)) -> f64 {
//    println!("q1");
    let (x, y) = v;
    let x = *x as f64;
    let y = *y as f64;
    let h = ((x * x) + (y * y)).sqrt();
    let theta = (x / h).asin();
    theta
}

fn angle_q2(v: &(i32, i32)) -> f64 {
//    println!("q2");
    let (x, y) = v;
    let x = *x as f64;
    let y = *y as f64;
    let h = ((x * x) + (y * y)).sqrt();
    let theta = ((-y) / h).asin() + (PI / 2.0);
    theta
}

fn angle_q3(v: &(i32, i32)) -> f64 {
//    println!("q3");
    let (x, y) = v;
    let x = *x as f64;
    let y = *y as f64;
    let h = ((x * x) + (y * y)).sqrt();
    let theta = ((-x) / h).asin() + PI;
    theta
}

fn angle_q4(v: &(i32, i32)) -> f64 {
//    println!("q4");
    let (x, y) = v;
    let x = *x as f64;
    let y = *y as f64;
    let h = ((x * x) + (y * y)).sqrt();
    let theta = (y / h).asin() + (3.0 * PI / 2.0);
    theta
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example1() {
        let input = include_str!("example1");
        let field = Field::parse(&input);
        assert_eq!(field.num_visible_from(&(1, 0)), 7);
        assert_eq!(field.num_visible_from(&(4, 0)), 7);
        assert_eq!(field.num_visible_from(&(0, 2)), 6);
        assert_eq!(field.num_visible_from(&(1, 2)), 7);
        assert_eq!(field.num_visible_from(&(2, 2)), 7);
        assert_eq!(field.num_visible_from(&(3, 2)), 7);
        assert_eq!(field.num_visible_from(&(4, 2)), 5);
        assert_eq!(field.num_visible_from(&(4, 3)), 7);
        assert_eq!(field.num_visible_from(&(3, 4)), 8);
        assert_eq!(field.num_visible_from(&(4, 4)), 7);
    }

    #[test]
    fn test_example2() {
        let input = include_str!("example2");
        assert_eq!(solve(input), ((5, 8), 33));
    }

    #[test]
    fn test_example3() {
        let input = include_str!("example3");
        assert_eq!(solve(input), ((1, 2), 35));
    }

//    // this test is quite slow
//    #[test]
//    fn test_example5() {
//        assert_eq!(solve("example5"), ((11, 13), 210));
//    }

    #[test]
    fn test_angle() {
        let a = angle(&(0, 20));
        let b = angle(&(20, 1));
        assert!(a < b);
        let up = 0.0;
        let right = PI / 2.0;
        let down = PI;
        let left = 3.0 * PI / 2.0;

        assert!(angle(&(1, -1)) > right);
        assert!(angle(&(1, -1)) < down);

        assert!(angle(&(0, -1)) > right);
        assert!(angle(&(0, -1)) < left);

        assert!(angle(&(-1, 1)) > left);
        assert!(angle(&(-1, 1)) < 2.0 * PI);

        assert!(angle(&(-1, -1)) > down);
        assert!(angle(&(-1, -1)) < left);

        assert_eq!(angle(&(0, 1)), up);
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("part2_example");
        let mut f = Field::parse(&input);
        let asteroid = f.destroy();
        assert_eq!(asteroid[0], (8, 1));
        assert_eq!(asteroid[1], (9, 0));
        assert_eq!(asteroid[2], (9, 1));
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let (_, solution) = solve(input);
        assert_eq!(solution, 334);
        let solution = solve2(input);
        assert_eq!(solution, 1119);
    }
}
