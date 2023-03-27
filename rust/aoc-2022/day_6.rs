use std::collections::HashSet;

const INPUT: &[u8] = include_bytes!("day_6_input.txt");

fn get_start(values: &[u8], win_size: usize) -> Option<usize> {
    values
        .windows(win_size)
        .map(|s| s.iter().map(|c| *c).collect::<HashSet<u8>>())
        .enumerate()
        .filter(|(_, s)| s.len() == win_size)
        .next()
        .map(|(i, _)| i + win_size)
}

fn main() {
    println!("{}", get_start(INPUT, 14).unwrap());
}
