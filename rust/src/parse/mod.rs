use nom::character::complete::{char, digit1, space0};
use nom::combinator::opt;
use nom::IResult;

pub fn parse_i32(input: &str) -> IResult<&str, i32> {
    let (input, _) = space0(input)?;
    let (input, sign) = opt(char('-'))(input)?;
    let (input, digits) = digit1(input)?;
    let n: i32 = digits.parse().unwrap();
    let n: i32 = match sign {
        Some(_) => -n,
        None => n
    };
    Ok((input, n))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_i32() {
        let result = parse_i32("-42, abc");
        let expected = Ok((", abc", -42));
        assert_eq!(result, expected)
    }
}
