fn main() {
    let a = Point::create(Position { x: 0, y: 0 }, Vector { x: 1, y: 1 });
    println!("{:?}", a);
}

#[derive(Debug, Eq, PartialEq)]
struct Position {
    x: i32,
    y: i32,
}

#[derive(Debug, Eq, PartialEq)]
struct Vector {
    x: i32,
    y: i32,
}

#[derive(Debug, Eq, PartialEq)]
struct Point {
    position: Position,
    direction: Vector,
}

impl Point {
    fn create(position: Position, direction: Vector) -> Point {
        Point { position, direction }
    }
}

fn parse(line: &str) -> Point {
    unimplemented!()
}

fn parse_i32(input: &str) -> i32 {
    unimplemented!()
}

#[derive(Debug, Eq, PartialEq)]
enum ParseResult<T> {
    Success { value: T, remainder: String },
    Fail { message: String },
}

fn success<T>(value: T, remainder: &str) -> ParseResult<T> {
    ParseResult::Success { value, remainder: String::from(remainder) }
}

fn fail<T>(message: &str) -> ParseResult<T> {
    ParseResult::Fail { message: String::from(message) }
}

struct Parser<T> {
    f: Box<dyn Fn(&str) -> ParseResult<T>>
}

impl<T> Parser<T> {
    fn parse(&self, s: &str) -> ParseResult<T> {
        (*self.f)(s)
    }
}

fn char() -> Parser<char> {
    let f = |x: &str| match x.len() {
        0 => fail("cannot parse empty string into char"),
        _ => success(x.char_indices().nth(0), x.chars().)
    };

    return Parser { f: Box::new(f) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_char() {
        let result: ParseResult<char> = char().parse("abc");
        let expected = success('a', "bc");
        assert_eq!(result, expected)
    }

    #[test]
    fn test_parse_point() {
        let example = "position=< 9,  1> velocity=< 0,  2";
        let point = parse(example);
        let expected = Point::create(Position { x: 9, y: 1 }, Vector { x: 0, y: 2 });
        assert_eq!(point, expected);
    }
}
