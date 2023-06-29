#![allow(unused_variables, dead_code)]

mod parse;
mod norm;
mod face;
mod core;

use std::io;
use std::io::Write;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("> ");
        let mut line = String::new();
        stdout.flush()?;
        stdin.read_line(&mut line)?;
        line = line.trim_end().to_owned();
        let (_, parsed) = parse::parse::parse_program(&line).unwrap();
        let normed = norm::Expr::from(parsed.expr);
        let normalized = norm::Expr::normalize(norm::Environment::new(), normed).unwrap();
        let (elaborated, _) = face::Expr::elaborate(face::Context::new(), face::Environment::new(), normalized).unwrap();
        let evalauted = core::Expr::evaluate(core::Environment::new(), elaborated).unwrap();
        println!("{:?}", evalauted);
    }
}
