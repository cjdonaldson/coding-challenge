use std::env;
// use std::vec::Vec;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Hello, world!, {:?}", args);
    // println(CalcOps(" sqrt 4 / 2"))
}
