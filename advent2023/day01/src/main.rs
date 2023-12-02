use std::cmp::min;

fn first_digit(s: &str) -> Option<u32> {
    for c in s.chars() {
        if c.is_digit(10) {
            return Some(c.to_digit(10).unwrap());
        }
    }
    return None;
}

fn last_digit(s: &str) -> Option<u32> {
    for c in s.chars().rev() {
        if c.is_digit(10) {
            return Some(c.to_digit(10).unwrap());
        }
    }
    return None;
}

const NUMBER_STR: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn first_digit_2(s: &str) -> Option<u32> {
    let chars = s.chars().collect::<Vec<char>>();
    for i in 0..s.len() {
        let c = chars.get(i).unwrap();
        if c.is_digit(10) {
            return Some(c.to_digit(10).unwrap());
        }
        let sub = &s[i..min(i + 5, s.len())];
        for (j, &n) in NUMBER_STR.iter().enumerate() {
            if sub.starts_with(n) {
                return Some((j + 1) as u32);
            }
        }
    }
    None
}

fn last_digit_2(s: &str) -> Option<u32> {
    let chars = s.chars().collect::<Vec<char>>();
    for i in (0..s.len()).rev() {
        let c = chars.get(i).unwrap();
        if c.is_digit(10) {
            return Some(c.to_digit(10).unwrap());
        }
        let sub = &s[i..min(i + 5, s.len())];
        for (j, &n) in NUMBER_STR.iter().enumerate() {
            if sub.starts_with(n) {
                return Some((j + 1) as u32);
            }
        }
    }
    None
}

fn main() {
    let input = std::fs::read_to_string("data/input.txt").unwrap();
    let lines = input.lines();
    let mut total = 0;
    for l in lines.clone() {
        if let (Some(x), Some(y)) = (first_digit(l), last_digit(l)) {
            total += 10 * x + y;
        }
    }
    println!("{}", total);
    let mut total = 0;
    for l in lines {
        if let (Some(x), Some(y)) = (first_digit_2(l), last_digit_2(l)) {
            total += 10 * x + y;
        }
    }
    println!("{}", total);
}
