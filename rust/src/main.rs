mod cal2018;
mod cal2019;
mod graph;
mod parse;

fn main() {
    cal2018::problem10::solve().unwrap();
    cal2018::problem11::solve();
    cal2018::problem13::main();

    cal2019::day01::main();
    cal2019::day02::main();
    cal2019::day03::main();
    cal2019::day04::main();
    cal2019::day05::main();
    cal2019::day06::main();
}