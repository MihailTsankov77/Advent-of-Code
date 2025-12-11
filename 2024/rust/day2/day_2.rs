use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn filterLinePart1(Vec<i32> line) {
    for i in 1..line.length() {
        
    }
}

fn part1() -> io::Result<()> {
    let mut list: Vec<Vec<i32>> = Vec::new();
    let file = File::open("day2/input.txt")?;
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        let line = line?;
        list.push(
            line.split_whitespace()
                .map(|x| x.parse::<i32>().unwrap())
                .collect(),
        );
    }

    println!(
        "Result part 1: {}",
        list
            .iter()
            .filter(filterLinePart1)
            .collect()
            .length()
    );

    Ok(())
}

fn main() -> io::Result<()> {
    part1();
    Ok(())
}
