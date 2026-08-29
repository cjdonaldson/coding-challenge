pub mod bounded_cell;
pub mod cell;
pub mod cell_error;
pub mod direction;
pub mod player;

use player::Player;
use player::display_player_chit;

pub const BOARD_WIDTH: usize = 7;
pub const BOARD_HEIGHT: usize = 6;

pub type BoardGrid = [[Player; BOARD_WIDTH]; BOARD_HEIGHT];

pub struct Board {
    pub grid: BoardGrid,
}

impl Board {
    pub fn new() -> Self {
        Board {
            grid: [[Player::None; BOARD_WIDTH]; BOARD_HEIGHT],
        }
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_column_index(f)?;
        display_grid(self.grid, f)
    }
}

fn display_column_index(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for col in 0..BOARD_WIDTH {
        write!(f, " {} ", col + 1)?;
    }
    writeln!(f)
}

fn display_grid(grid: BoardGrid, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for row in grid {
        let row_str: String = row
            .iter()
            .map(|&player| display_player_chit(player))
            .collect::<Vec<String>>()
            .join(" ");

        writeln!(f, "{}", row_str)?;
    }
    Ok(())
}
