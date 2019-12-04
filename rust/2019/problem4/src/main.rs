fn main() {
    let range = 153517..(630395+1);
    let a = range.clone()
        .filter(|n| valid(*n))
        .count();

    println!("The solution to part A is {}", a);

    let b = range.clone()
        .filter(|n| valid2(*n))
        .count();

    println!("The solution to part B is {}", b);

}

//It is a six-digit number.
//The value is within the range given in your puzzle input.
//Two adjacent digits are the same (like 22 in 122345).
//Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
fn valid(n: i32) -> bool {
    let digits = get_digits(n);
    if !has_double_adjacent_digits(&digits) {
        false
    } else if !increasing(&digits) {
        false
    } else {
        true
    }
}

fn valid2(n: i32) -> bool {
    let digits = get_digits(n);

    if !has_double_adjacent_digits(&digits) {
        false
    } else if !has_at_least_one_pair_of_adjacent_digits(&digits) {
        false
    } else if !increasing(&digits) {
        false
    } else {
        true
    }
}

fn has_at_least_one_pair_of_adjacent_digits(n: &Digits) -> bool {
    let Digits((a, b, c, d, e, f)) = n;

    if a == b && b != c {
        true
    } else if b == c && a != b && c != d {
        true
    } else if c == d && b != c && d != e {
        true
    } else if d == e && c != d && e != f{
        true
    } else if e == f && d != e {
        true
    } else {
        false
    }
}

struct Digits((i32, i32, i32, i32, i32, i32));

fn get_digits(n: i32) -> Digits {
    let a = n / 100000;
    let b = (n / 10000) % 10;
    let c = (n / 1000) % 10;
    let d = (n / 100) % 10;
    let e = (n / 10) % 10;
    let f = n % 10;

    Digits((a, b, c, d, e, f))
}

fn has_double_adjacent_digits(n: &Digits) -> bool {
    let Digits((a, b, c, d, e, f)) = n;

    if a == b {
        true
    } else if b == c {
        true
    } else if c == d {
        true
    } else if d == e {
        true
    } else if e == f {
        true
    } else {
        false
    }
}

fn increasing(n: &Digits) -> bool {
    let Digits((a, b, c, d, e, f)) = n;

    if a > b {
        false
    } else if b > c {
        false
    } else if c > d {
        false
    } else if d > e {
        false
    } else if e > f {
        false
    } else {
        true
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_valid() {
        //111111 meets these criteria (double 11, never decreases).
        assert_eq!(valid(111111), true);
        //223450 does not meet these criteria (decreasing pair of digits 50).
        assert_eq!(valid(223450), false);
        //123789 does not meet these criteria (no double).
        assert_eq!(valid(123789), false);
    }

    #[test]
    fn test_valid2() {
        //112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
        assert!(valid2(112233));
        //123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
        assert!(!valid2(123444));
        //111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
        assert!(valid2(111122));
    }
}
