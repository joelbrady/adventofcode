pub fn solve() {
    let input = 4842;

    let grid: Grid = Grid::from_seed(input);

    let (x, y, _) = grid.highest_power_square(3);
    println!("The solution to part a for input {} is ({},{})", input, x, y);
    let (x, y, size) = grid.highest_square_power_any_size();
    println!("The solution to part b for input {} is ({},{},{})", input, x, y, size);
}

struct Grid {
    grid: [[i32; 300]; 300]
}

impl Grid {
    fn from_seed(seed: i32) -> Grid {
        let mut grid = [[0; 300]; 300];

        for y in 1..301 {
            for x in 1..301 {
                let power_level = generate(seed, x, y);
                let iy = (y - 1) as usize;
                let ix = (x - 1) as usize;
                grid[ix][iy] = power_level;
            }
        }

        Grid { grid }
    }

    fn at(&self, x: i32, y: i32) -> i32 {
        let iy = (y - 1) as usize;
        let ix = (x - 1) as usize;
        self.grid[ix][iy]
    }

    fn square_power(&self, x: i32, y: i32, size: i32) -> i32 {
        let mut sum = 0;
        for ix in x..(x + size) {
            for iy in y..(y + size) {
                let n = self.at(ix, iy);
                sum += n;
            }
        }
        sum
    }

    fn highest_power_square(&self, size: i32) -> (i32, i32, i32) {
        let mut highest_sum = self.square_power(1, 1, size);
        let mut highest_square: (i32, i32) = (1, 1);
        for x in 1..(301 - size + 1) {
            for y in 1..(301 - size + 1) {
                let power = self.square_power(x, y, size);
                if power > highest_sum {
                    highest_sum = power;
                    highest_square = (x, y);
                }
            }
        }
        let (x, y) = highest_square;
        (x, y, highest_sum)
    }

    fn highest_square_power_any_size(&self) -> (i32, i32, i32) {
        let (mut x, mut y, mut highest_power) = self.highest_power_square(1);
        let mut highest_size = 1;

        for size in 2..301 {
            let (xx, yy, pp) = self.highest_power_square(size);
            if pp > highest_power {
                highest_power = pp;
                highest_size = size;
                x = xx;
                y = yy;
            }
        }

        (x, y, highest_size)
    }
}

fn generate(seed: i32, x: i32, y: i32) -> i32 {
    //Find the fuel cell's rack ID, which is its X coordinate plus 10.
    let rack_id = x + 10;
    //Begin with a power level of the rack ID times the Y coordinate.
    let power_level = rack_id * y;
    //Increase the power level by the value of the grid serial number (your puzzle input).
    let power_level = power_level + seed;
    //Set the power level to itself multiplied by the rack ID.
    let power_level = power_level * rack_id;
    //Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
    let power_level = hundreds_digit(power_level);
    //Subtract 5 from the power level.
    let power_level = power_level - 5;
    power_level
}

fn hundreds_digit(n: i32) -> i32 {
    (n / 100) % 10
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate() {
        assert_eq!(generate(8, 3, 5), 4);
        //Fuel cell at  122,79, grid serial number 57: power level -5.
        assert_eq!(generate(57, 122, 79), -5);
        //Fuel cell at 217,196, grid serial number 39: power level  0.
        assert_eq!(generate(39, 217, 196), 0);
        //Fuel cell at 101,153, grid serial number 71: power level  4.
        assert_eq!(generate(71, 101, 153), 4);
        assert_eq!(generate(18, 33, 45), 4);
        assert_eq!(generate(18, 34, 45), 4);
        assert_eq!(generate(18, 35, 45), 4);
        assert_eq!(generate(18, 36, 45), -5);
    }

    #[test]
    fn test_example_grid() {
        let grid = Grid::from_seed(18);
        assert_eq!(grid.at(33, 45), 4);
        assert_eq!(grid.square_power(33, 45, 3), 29);
        assert_eq!(grid.highest_power_square(3), (33, 45, 29));
        //For grid serial number 18, the largest total square (with a total power of 113)
        // is 16x16 and has a top-left corner of 90,269, so its identifier is 90,269,16.
        assert_eq!(grid.square_power(90, 269, 16), 113);
        assert_eq!(grid.highest_power_square(16), (90, 269, 113));
        // too slow to run in tests
//        assert_eq!(grid.highest_square_power_any_size(), (90, 269, 16));
    }

    #[test]
    fn test_second_example_grid() {
        //For grid serial number 42, the largest 3x3 square's top-left is 21,61
        // (with a total power of 30)
        let grid = Grid::from_seed(42);
        assert_eq!(grid.square_power(21, 61, 3), 30);
        assert_eq!(grid.highest_power_square(3), (21, 61, 30));
    }

    #[test]
    fn test_hundreds_digit() {
        assert_eq!(hundreds_digit(12345), 3);
        assert_eq!(hundreds_digit(1), 0);
        assert_eq!(hundreds_digit(12045), 0);
        assert_eq!(hundreds_digit(1000000), 0);
    }

    #[test]
    fn solve_puzzle() {
        solve()
    }
}
