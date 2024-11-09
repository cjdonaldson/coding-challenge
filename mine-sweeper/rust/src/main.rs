mod board;
mod game_rules;
use game_rules::make_move;

use board::Board;

use std::io;
use std::thread::sleep;
use std::time::Duration;

use board::board_properties::Difficulty;

mod bomb_count;

mod select_bomb_locations;

const BOARD_HEIGHT: usize = 6;
const BOARD_WIDTH: usize = 7;

fn get_user_move() -> String {
    let mut user_move = String::new();
    println!("enter cell location (ie: a0), easy, medium, hard or quit: ");
    io::stdin()
        .read_line(&mut user_move)
        .expect("Failed to read line");

    user_move.trim().to_owned()
}

fn main() {
    let mut board = Board::new(BOARD_HEIGHT, BOARD_WIDTH, Difficulty::Hard);
    loop {
        println!("{}", board);

        let user_input: String = get_user_move().to_ascii_uppercase();

        let is_quit = || ["Q", "QUIT"].contains(&user_input.as_str());
        let is_easy = || ["E", "EASY"].contains(&user_input.as_str());
        let is_med = || ["M", "MED", "MEDIUM"].contains(&user_input.as_str());
        let is_hard = || ["H", "HARD"].contains(&user_input.as_str());

        if is_quit() {
            break;
        } else if is_easy() {
            board = Board::new(BOARD_HEIGHT, BOARD_WIDTH, Difficulty::Easy);
        } else if is_med() {
            board = Board::new(BOARD_HEIGHT, BOARD_WIDTH, Difficulty::Medium);
        } else if is_hard() {
            board = Board::new(BOARD_HEIGHT, BOARD_WIDTH, Difficulty::Hard);
        } else {
            fn bad_move(user_move: &str) {
                println!("invalid selection: {user_move}");
                sleep(Duration::new(5, 0));
            }

            match make_move(&user_input, &mut board) {
                Ok(_) => (),
                Err(e) => {
                    println!("issue {e}");
                    bad_move(&user_input)
                }
            }
        }
    }
    println!("thanks for playing");
}
