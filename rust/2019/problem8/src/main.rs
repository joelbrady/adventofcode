use input::get_input;

fn main() {
    let input = get_input("input");
    let input = parse(&input);
    let solution = solve(&input, 25, 6);
    println!("the solution to part 1 is {}", solution);

    print_part2_solution(&input, 25, 6);
}

#[derive(Copy, Clone)]
enum Color {
    Transparent,
    Black,
    White,
}

impl From<i32> for Color {
    fn from(n: i32) -> Self {
        use Color::*;

        match n {
            2 => Transparent,
            1 => White,
            0 => Black,
            _ => unimplemented!(),
        }
    }
}

fn print_part2_solution(pixels: &[i32], width: i32, height: i32) {
    use Color::*;

    let pixels: Vec<Color> = pixels.iter()
        .map(|n| Color::from(*n))
        .collect();

    for y in 0..height {
        for x in 0..width {
            let pixel = get_color(&pixels, width, height, x, y);
            match pixel {
                Black => print!(" "),
                White => print!("#"),
                _ => unimplemented!(),
            }
        }
        println!();
    }
}

fn get_color(pixels: &[Color], width: i32, height: i32, x: i32, y: i32) -> Color {
    use Color::*;

    let depth = pixels.len() as i32 / (width * height);
    for z in 0..depth {
        let p = get_pixel(pixels, z, y, x, width, height);
        match p {
            Transparent => continue,
            Black => return Black,
            White => return White,
        }
    }

    panic!("entire pixels is transparent!")
}

fn parse(s: &str) -> Vec<i32> {
    s.bytes()
        .map(|c| String::from_utf8(vec![c]).unwrap())
        .map(|digit| digit.parse::<i32>().unwrap())
        .collect()
}

fn solve(pixels: &[i32], width: i32, height: i32) -> i32 {
    let depth = pixels.len() / (width * height) as usize;

    let mut least_zeros = num_zeroes_in_layer(pixels, 0, width, height);
    let mut layer_with_least_zeroes = 0;
    for layer in 1..depth {
        let n = num_zeroes_in_layer(pixels, layer as i32, width, height);
        if n < least_zeros {
            least_zeros = n;
            layer_with_least_zeroes = layer;
        }
    }

    checksum(pixels, layer_with_least_zeroes as i32, width, height)
}

fn checksum(pixels: &[i32], z: i32, width: i32, height: i32) -> i32 {
    let mut ones = 0;
    let mut twos = 0;
    for y in 0..height {
        for x in 0..width {
            let p = get_pixel(pixels, z, y, x, width, height);
            if p == 1 {
                ones += 1;
            }
            if p == 2 {
                twos += 1;
            }
        }
    }

    ones * twos
}

fn num_zeroes_in_layer(pixels: &[i32], layer: i32, width: i32, height: i32) -> i32 {
    let mut num_zeroes = 0;
    for y in 0..height {
        for x in 0..width {
            let p = get_pixel(pixels, layer, y, x, width, height);
            if p == 0 {
                num_zeroes += 1;
            }
        }
    }

    num_zeroes
}

fn get_pixel<T>(input: &[T], z: i32, y: i32, x: i32, width: i32, height: i32) -> T
    where T: Copy {
    let layer_size = width * height;

    let index = ((z * layer_size) + (y * width) + x) as usize;
    input[index]
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse("1234"), vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_example() {
        let input = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2];
        let width = 3;
        let height = 2;

        assert_eq!(get_pixel(&input, 0, 1, 1, width, height), 5);

        assert_eq!(get_pixel(&input, 1, 1, 0, width, height), 0);
        assert_eq!(get_pixel(&input, 1, 1, 1, width, height), 1);
        assert_eq!(get_pixel(&input, 1, 1, 2, width, height), 2);

        let solution = solve(&input, width, height);
        assert_eq!(solution, 1);
    }
}
