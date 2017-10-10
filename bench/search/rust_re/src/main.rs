extern crate regex;
use regex::Regex;
use std::io::prelude::*;
use std::fs::File;
use std::env;
use std::time::Instant;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
		println!("Usage: rust_r <re> <file> [print]>\n");
		return;
	}
	let re = Regex::new(&args[1]).unwrap();
    let mut file = File::open(&args[2]).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
	
	let mut count = 0;
    let start = Instant::now();
    if args.len() == 4 && args[3] == "print" {
        for m in re.find_iter(&contents) {
            println!("{:?}\n", m);
            count += 1;
        }
    }
    else {
        for _ in re.find_iter(&contents) {
            count += 1;
        }
    }
    let time = start.elapsed();
	println!("\n\nTotal matches {:?}\nTime elapsed {:?} sec\n", count, time.as_secs() as f64 + (time.subsec_nanos() as f64)*1e-9);
}
