mod board;
mod color;
mod game;
mod rules;

use board::*;
use game::Game;
use rules::move_error::{ErrorMessage, MoveError, PlayerInput};

use std::io;

fn main() {
    let mut game = Game::default();
    println!("{}", game);

    while game.continue_play() {
        let user_move = get_move_column();

        let player_move = match user_input_to_column(user_move, &game) {
            Some(value) => value,
            None => continue,
        };

        match game.play_move(player_move - 1) {
            Ok(_) => {
                println!("{}", game);
            }
            Err(error) => {
                game.display_error(error.to_string());
            }
        }
    }
}

fn user_input_to_column(user_move: String, game: &Game) -> Option<usize> {
    match validate_input_to_column(&user_move) {
        Ok(moved) => Some(moved),
        Err(error) => {
            game.display_error(error.to_string());
            None
        }
    }
}

fn get_move_column() -> String {
    let mut user_move = String::new();
    io::stdin()
        .read_line(&mut user_move)
        .expect("Failed to read line");

    user_move
}

fn validate_input_to_column(player_move: &str) -> Result<usize, MoveError> {
    let column = player_move.to_string().trim().parse::<usize>();
    match column {
        Ok(number) => {
            if (1..=BOARD_WIDTH).contains(&number) {
                Ok(number)
            } else {
                Err(MoveError::InvalidColumn(player_move.to_string()))
            }
        }
        Err(error) => Err(MoveError::ParseError(
            ErrorMessage(error.to_string()),
            PlayerInput(player_move.to_string()),
        )),
    }
}
