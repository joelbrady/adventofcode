use std::fs::File;
use std::io::Read;

pub fn get_input(filename: &str) -> String {
    let mut string = String::new();
    File::open(filename)
        .unwrap()
        .read_to_string(&mut string)
        .unwrap();

    string
}

#[cfg(test)]
mod tests {
    use crate::get_input;

    #[test]
    fn it_works() {
        let input = get_input("example");
        let expected = "example\n";
        assert_eq!(input.as_str(), expected);
    }
}
