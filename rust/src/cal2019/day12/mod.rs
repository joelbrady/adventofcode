use std::ops::Add;

pub fn main() {
    println!("The solution to part 1 is {}", part1());
    println!("The solution to part 2 is {}", part2())
}

fn part1() -> i64 {
    //<x=-19, y=-4, z=2>
    //<x=-9, y=8, z=-16>
    //<x=-4, y=5, z=-11>
    //<x=1, y=9, z=-13>
    let a = Moon::from_tuple((-19, -4, 2));
    let b = Moon::from_tuple((-9, 8, -16));
    let c = Moon::from_tuple((-4, 5, -11));
    let d = Moon::from_tuple((1, 9, -13));

    let (a, b, c, d) = steps(1000, &a, &b, &c, &d);
    total_energy(&a, &b, &c, &d)
}

fn part2() -> u64 {
    let a = Moon::from_tuple((-19, -4, 2));
    let b = Moon::from_tuple((-9, 8, -16));
    let c = Moon::from_tuple((-4, 5, -11));
    let d = Moon::from_tuple((1, 9, -13));

    solve2(&a, &b, &c, &d)
}

fn solve2(a: &Moon, b: &Moon, c: &Moon, d: &Moon) -> u64 {
    let x = steps_until_repeat(&a, &b, &c, &d, |m| m.velocity.x);
    let y = steps_until_repeat(&a, &b, &c, &d, |m| m.velocity.y);
    let z = steps_until_repeat(&a, &b, &c, &d, |m| m.velocity.z);

    lcm(x, y, z) * 2
}

fn steps_until_repeat<F>(a: &Moon, b: &Moon, c: &Moon, d: &Moon, f: F) -> u64
    where F: Fn(&Moon) -> i64 {
    let mut steps: u64 = 0;
    let mut a = a.clone();
    let mut b = b.clone();
    let mut c = c.clone();
    let mut d = d.clone();

    loop {
        let ms = step(&a, &b, &c, &d);
        a = ms.0;
        b = ms.1;
        c = ms.2;
        d = ms.3;
        steps += 1;
        if f(&a) == 0 && f(&b) == 0 && f(&c) == 0 && f(&d) == 0 {
            break;
        }
    }

    steps
}

fn total_energy(a: &Moon, b: &Moon, c: &Moon, d: &Moon) -> i64 {
    vec![a, b, c, d].iter().map(|m| m.total_energy()).sum()
}

fn step(a: &Moon, b: &Moon, c: &Moon, d: &Moon) -> (Moon, Moon, Moon, Moon) {
    let new_a = a.step(&vec![b, c, d]);
    let new_b = b.step(&vec![a, c, d]);
    let new_c = c.step(&vec![a, b, d]);
    let new_d = d.step(&vec![a, b, c]);

    (new_a, new_b, new_c, new_d)
}

fn steps(n: u64, a: &Moon, b: &Moon, c: &Moon, d: &Moon) -> (Moon, Moon, Moon, Moon) {
    if n == 0 {
        (a.clone(), b.clone(), c.clone(), d.clone())
    } else {
        let (a, b, c, d) = step(a, b, c, d);
        steps(n - 1, &a, &b, &c, &d)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct Moon {
    position: Position,
    velocity: Vector,
}

impl Moon {
    fn from_tuple(tuple: (i64, i64, i64)) -> Self {
        Moon { position: Position::from(tuple), velocity: Vector::zero() }
    }

    fn step(&self, ms: &[&Moon]) -> Moon {
        let velocity = self.calculate_velocity(ms);
        let position = self.position + velocity;
        Moon { position, velocity }
    }

    fn calculate_velocity(&self, ms: &[&Moon]) -> Vector {
        let (x, y, z) = self.position.as_tuple();
        let delta: Vector = ms.iter()
            .map(|m| {
                let (xx, yy, zz) = m.position.as_tuple();

                let dx = cmp(x, xx);
                let dy = cmp(y, yy);
                let dz = cmp(z, zz);

                Vector::from((dx, dy, dz))
            })
            .fold(Vector::from((0, 0, 0)), |acc, x| acc + x);

        self.velocity + delta
    }

    fn potential_energy(&self) -> i64 {
        //A moon's potential energy is the sum of the absolute values of its x, y, and z position coordinates.
        let (x, y, z) = self.position.as_tuple();
        x.abs() + y.abs() + z.abs()
    }

    fn kinetic_energy(&self) -> i64 {
        // A moon's kinetic energy is the sum of the absolute values of its velocity coordinates.
        let (x, y, z) = self.velocity.as_tuple();
        x.abs() + y.abs() + z.abs()
    }

    fn total_energy(&self) -> i64 {
        self.potential_energy() * self.kinetic_energy()
    }
}

fn cmp(a: i64, b: i64) -> i64 {
    if a < b {
        1
    } else if a > b {
        -1
    } else {
        0
    }
}

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
struct Position {
    x: i64,
    y: i64,
    z: i64,
}

impl Position {
    fn as_tuple(&self) -> (i64, i64, i64) {
        (self.x, self.y, self.z)
    }
}

impl From<(i64, i64, i64)> for Position {
    fn from(tuple: (i64, i64, i64)) -> Self {
        let (x, y, z) = tuple;
        Position { x, y, z }
    }
}

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
struct Vector {
    x: i64,
    y: i64,
    z: i64,
}

impl From<(i64, i64, i64)> for Vector {
    fn from(tuple: (i64, i64, i64)) -> Self {
        let (x, y, z) = tuple;
        Vector { x, y, z }
    }
}

impl Vector {
    fn zero() -> Vector {
        Vector { x: 0, y: 0, z: 0 }
    }

    fn as_tuple(&self) -> (i64, i64, i64) {
        (self.x, self.y, self.z)
    }
}

impl Add<Vector> for Position {
    type Output = Position;

    fn add(self, rhs: Vector) -> Self::Output {
        Position { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
    }
}

impl Add<Vector> for Vector {
    type Output = Vector;

    fn add(self, rhs: Vector) -> Self::Output {
        Vector { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
    }
}

trait Abs {
    fn abs(self) -> i64;
}

impl Abs for i64 {
    fn abs(self) -> i64 {
        if self < 0 {
            -self
        } else {
            self
        }
    }
}

fn lcm(a: u64, b: u64, c: u64) -> u64 {
    let mut aa = a;
    let mut bb = b;
    let mut cc = c;
    loop {
        if aa == bb && bb == cc {
            return aa;
        } else if aa < bb || aa < cc {
            aa += a;
        } else if bb < aa || bb < cc {
            bb += b;
        } else {
            cc += c;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        //<x=-1, y=0, z=2>
        //<x=2, y=-10, z=-7>
        //<x=4, y=-8, z=8>
        //<x=3, y=5, z=-1>
        let a = Moon::from_tuple((-1, 0, 2));
        let b = Moon::from_tuple((2, -10, -7));
        let c = Moon::from_tuple((4, -8, 8));
        let d = Moon::from_tuple((3, 5, -1));

        let (a, b, c, d) = step(&a, &b, &c, &d);

        //pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
        //pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
        //pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
        //pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>
        assert_eq!(a.velocity, Vector::from((3, -1, -1)));
        assert_eq!(b.velocity, Vector::from((1, 3, 3)));
        assert_eq!(c.velocity, Vector::from((-3, 1, -3)));
        assert_eq!(d.velocity, Vector::from((-1, -3, 1)));

        assert_eq!(a.position, Position::from((2, -1, 1)));
        assert_eq!(b.position, Position::from((3, -7, -4)));
        assert_eq!(c.position, Position::from((1, -7, 5)));
        assert_eq!(d.position, Position::from((2, 2, 0)));
    }

    #[test]
    fn test_example_step10() {
        let a = Moon::from_tuple((-1, 0, 2));
        let b = Moon::from_tuple((2, -10, -7));
        let c = Moon::from_tuple((4, -8, 8));
        let d = Moon::from_tuple((3, 5, -1));

        //pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
        //pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
        //pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
        //pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
        let (a, b, c, d) = steps(10, &a, &b, &c, &d);

        assert_eq!(a.velocity, Vector::from((-3, -2, 1)));
        assert_eq!(b.velocity, Vector::from((-1, 1, 3)));
        assert_eq!(c.velocity, Vector::from((3, 2, -3)));
        assert_eq!(d.velocity, Vector::from((1, -1, -1)));

        assert_eq!(a.position, Position::from((2, 1, -3)));
        assert_eq!(b.position, Position::from((1, -8, 0)));
        assert_eq!(c.position, Position::from((3, -6, 1)));
        assert_eq!(d.position, Position::from((2, 0, 4)));
    }

    #[test]
    fn test_example_energy_step10() {
        //Energy after 10 steps:
        //pot: 2 + 1 + 3 =  6;   kin: 3 + 2 + 1 = 6;   total:  6 * 6 = 36
        //pot: 1 + 8 + 0 =  9;   kin: 1 + 1 + 3 = 5;   total:  9 * 5 = 45
        //pot: 3 + 6 + 1 = 10;   kin: 3 + 2 + 3 = 8;   total: 10 * 8 = 80
        //pot: 2 + 0 + 4 =  6;   kin: 1 + 1 + 1 = 3;   total:  6 * 3 = 18
        //Sum of total energy: 36 + 45 + 80 + 18 = 179

        let a = Moon::from_tuple((-1, 0, 2));
        let b = Moon::from_tuple((2, -10, -7));
        let c = Moon::from_tuple((4, -8, 8));
        let d = Moon::from_tuple((3, 5, -1));

        let (a, b, c, d) = steps(10, &a, &b, &c, &d);

        assert_eq!(a.potential_energy(), 6);
        assert_eq!(b.potential_energy(), 9);
        assert_eq!(c.potential_energy(), 10);
        assert_eq!(d.potential_energy(), 6);

        assert_eq!(a.kinetic_energy(), 6);
        assert_eq!(b.kinetic_energy(), 5);
        assert_eq!(c.kinetic_energy(), 8);
        assert_eq!(d.kinetic_energy(), 3);

        assert_eq!(total_energy(&a, &b, &c, &d), 179);
    }

    #[test]
    fn test_part2_example1() {
        let a = Moon::from_tuple((-1, 0, 2));
        let b = Moon::from_tuple((2, -10, -7));
        let c = Moon::from_tuple((4, -8, 8));
        let d = Moon::from_tuple((3, 5, -1));

        let solution = solve2(&a, &b, &c, &d);
        assert_eq!(solution, 2772);
    }

    #[test]
    fn test_lcm() {
        assert_eq!(lcm(1, 2, 3), 6);
    }

    #[test]
    fn test_solution() {
        assert_eq!(part1(), 8287);
        assert_eq!(part2(), 528250271633772);
    }
}
