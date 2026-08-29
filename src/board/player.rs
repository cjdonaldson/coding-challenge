use super::super::color::*;

const USE_DISPLAY_PLAYER_CHIT_NUMBER: bool = false;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Player {
    None,
    One,
    Two,
}

impl std::fmt::Display for Player {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Player::None => write!(f, " ."),
            Player::One => write!(f, "{}{}{}", ORANGE, select_chit(1), RESET),
            Player::Two => write!(f, "{}{}{}", BLUE, select_chit(2), RESET),
        }
    }
}

fn select_chit(number: usize) -> String {
    if USE_DISPLAY_PLAYER_CHIT_NUMBER {
        number.to_string()
    } else {
        "🟡".to_string()
    }
}

pub fn display_winner_p(player: Player, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match player {
        Player::None => writeln!(f, "{}It's a draw!{}", RED, RESET),
        Player::One => writeln!(f, "{}Player {} has won!{}", ORANGE, player, RESET),
        Player::Two => writeln!(f, "{}Player {} has won!{}", BLUE, player, RESET),
    }
}

pub fn display_player_chit(player: Player) -> String {
    format!("{}", player)
}
