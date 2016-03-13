extern crate rand;
use std::io::{self, Write};
use rand::Rng;
static MAX_NUMBER: i32 = 101;
fn play_game() {
    println!("Guess the number!");
    let number = rand::thread_rng().gen_range(1, MAX_NUMBER);
    let mut guess_str = String::new();
    let mut guess: i32;
    loop {
        print!("Input your guess: ");
        io::stdout().flush().unwrap();
        guess_str.clear();
        io::stdin().read_line(&mut guess_str).
            expect("Failed to read a number!");
        guess = guess_str.trim().parse().
            expect(&(format!("{} is not a number", guess_str)));
        if guess > number {
            println!("Too big");
        } else if guess < number {
            println!("Too small");
        } else {
            println!("you win");
            break;
        }
    }
}
fn main() {
    let mut again = String::new();
    loop {
        play_game();
        print!("Play again? (Y/N): ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut again).expect("");
        if again.trim() != "Y" && again.trim() != "y" {
            break;
        }
    }
    println!("Thanks for playing!");
}
