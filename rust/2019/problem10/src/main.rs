use std::collections::HashSet;
use num_integer::gcd;
use input::get_input;

fn main() {
    let (_, solution) = solve("input");
    println!("the solution to part 1 is {}", solution);
}

fn solve(filename: &str) -> ((i32, i32), i32) {
    let input = get_input(filename);
    let f = Field::parse(&input);
    f.largest_visibility()
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
                _ => panic!("unexpected char"),
            };
        }
        Field { asteroids }
    }

    fn largest_visibility(&self) -> ((i32, i32), i32) {
        let mut asteroid: &(i32, i32) = &(-1, -1);
        let mut largest = -1;

        for a in self.asteroids.iter() {
            let v = self.visible_from(a);
            if v > largest {
                largest = v;
                asteroid = a;
            }
        }

        (*asteroid, largest)
    }

    fn visible_from(&self, a: &(i32, i32)) -> i32 {
        let mut visible = self.asteroids.clone();
        for b in self.asteroids.iter() {
            if a == b {
                continue;
            }
            let d = sub(b, a);
            let d = simplify(&d);
            let multiples = self.multiples_in_bounds(b, &d);
            if *a == (4, 2) {
            }
            multiples.iter()
                .for_each(|m| {
                    if visible.remove(m) {
                    }
                });
        }
        visible.remove(a);
        visible.len() as i32
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

#[cfg(test)]
mod test {
    use input::get_input;

    use super::*;

    #[test]
    fn test_example1() {
        let input = get_input("example1");
        let field = Field::parse(&input);
        assert_eq!(field.visible_from(&(1, 0)), 7);
        assert_eq!(field.visible_from(&(4, 0)), 7);
        assert_eq!(field.visible_from(&(0, 2)), 6);
        assert_eq!(field.visible_from(&(1, 2)), 7);
        assert_eq!(field.visible_from(&(2, 2)), 7);
        assert_eq!(field.visible_from(&(3, 2)), 7);
        assert_eq!(field.visible_from(&(4, 2)), 5);
        assert_eq!(field.visible_from(&(4, 3)), 7);
        assert_eq!(field.visible_from(&(3, 4)), 8);
        assert_eq!(field.visible_from(&(4, 4)), 7);
    }

    #[test]
    fn test_example2() {
        assert_eq!(solve("example2"), ((5, 8), 33));
    }

    #[test]
    fn test_example3() {
        assert_eq!(solve("example3"), ((1, 2), 35));
    }

//    #[test]
//    fn test_example5() {
//        assert_eq!(solve("example5"), ((11, 13), 210));
//    }
}
