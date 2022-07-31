mod data;
mod emit;
mod parse;
use crate::data::*;
use crate::emit::*;
use crate::parse::*;
use std::io::{self, BufRead};

fn main() {
    for line in io::stdin().lock().lines() {
        let l = line.unwrap();
        if let Ok((_, expr)) = expression(InputState::new(), &l) {
            println!("{}\t\t\t~~> {}", &l, expr.emit_keep_parens());
        } else {
            println!("{}\t\t\t~~> an error occured for this line", &l);
        }
    }
}
