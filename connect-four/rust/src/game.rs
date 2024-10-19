use tailcall::tailcall;

use crate::board::bounded_cell::BoundedCell;
use crate::board::cell::Cell;
use crate::board::direction::Direction;
use crate::board::player::*;
use crate::board::Board;
use crate::board::BoardGrid;
use crate::board::BOARD_HEIGHT;
use crate::board::BOARD_WIDTH;
use crate::color::*;
use crate::rules::move_error::MoveError;

pub struct Game {
    board: Board,
    move_counter: usize,
    current_player: Player,
    winner: Player,
}

impl Game {
    pub fn default() -> Game {
        Game {
            board: Board::new(),
            move_counter: 0,
            current_player: Player::One,
            winner: Player::None,
        }
    }

    pub fn display_winner(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let report_end =
            self.winner != Player::None || self.move_counter == BOARD_WIDTH * BOARD_HEIGHT;

        if report_end {
            writeln!(f)?;
            display_winner_p(self.winner, f)
        } else {
            Ok(())
        }
    }

    pub fn continue_play(&self) -> bool {
        !self.is_finished()
    }

    fn is_finished(&self) -> bool {
        self.winner != Player::None
    }

    pub fn play_move(&mut self, column: usize) -> Result<(), MoveError> {
        if self.winner != Player::None {
            return Err(MoveError::GameFinished);
        }

        if column >= BOARD_WIDTH {
            return Err(MoveError::InvalidColumn(column.to_string()));
        }

        if let Some(row) = (0..BOARD_HEIGHT)
            .rev()
            .find(|&row| self.board.grid[row][column] == Player::None)
        {
            self.board.grid[row][column] = self.current_player;

            self.move_counter += 1;
            self.winner = self.calculate_winner();

            self.current_player = match self.current_player {
                Player::One => Player::Two,
                _ => Player::One,
            };
        } else {
            return Err(MoveError::ColumnFull);
        }
        Ok(())
    }

    pub fn display_error(&self, error: String) {
        println!("{}Error: {}{}", RED, error, RESET);
    }

    fn player_at(&self, cell: &Cell) -> Player {
        player_at(&self.board.grid, cell)
    }

    fn test_player(&self, cell: &Cell) -> Player {
        let player: Player = self.player_at(cell);

        match player {
            Player::None => Player::None,
            _ => match Direction::ALL
                .map(|d| has_player_won(self.board.grid, player, *cell, d, 1))
                .into_iter()
                .find(|&p| p != Player::None)
            {
                None => Player::None,
                Some(p) => p,
            },
        }
    }

    fn calculate_winner(&self) -> Player {
        let bounded_cell: BoundedCell = BoundedCell::new(BOARD_HEIGHT, BOARD_WIDTH);

        for row in 0..BOARD_HEIGHT {
            for col in 0..BOARD_WIDTH {
                // intentional unwrap; row and col are in bounds; Err free
                let cell = &bounded_cell.cell_from(row as isize, col as isize).unwrap();
                let winner = self.test_player(cell);
                if winner == Player::None {
                    continue;
                } else {
                    return winner;
                }
            }
        }
        Player::None
    }
}

fn player_at(board: &BoardGrid, cell: &Cell) -> Player {
    board[cell.row][cell.col]
}

#[tailcall]
fn has_player_won(
    grid: BoardGrid,
    player: Player,
    cell: Cell,
    direction: Direction,
    count: u8,
) -> Player {
    let winning_sequence_length = 4;

    if count == winning_sequence_length {
        return player;
    } else {
        let bounded_cell: BoundedCell = BoundedCell::new(BOARD_HEIGHT, BOARD_WIDTH);

        match bounded_cell.cell_from_direction(&cell, &direction) {
            Err(_e) => Player::None,
            Ok(cell) => {
                if player_at(&grid, &cell) != player {
                    Player::None
                } else {
                    has_player_won(grid, player, cell, direction, count + 1)
                }
            }
        }
    }
}

impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        clear_terminal(f)?;
        display_board_edge(f)?;
        writeln!(
            f,
            "{}CONNECT 4 (Move {}){}",
            GREEN, self.move_counter, RESET
        )?;
        display_board_edge(f)?;
        write!(f, "{}", self.board)?;
        display_board_edge(f)?;
        write!(f, "{} move: ", self.current_player)?;
        self.display_winner(f)
    }
}

fn clear_terminal(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{esc}[2J{esc}[1;1H", esc = 27u8 as char)
}

fn display_board_edge(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "{}--------------------{}", GREEN, RESET)
}

#[cfg(test)]
mod tests {
    use super::Player;
    use super::*; // to pull in Game and Player // needed for tests

    #[test]
    fn player_one_row_1_one_short() {
        let mut game = Game::default();
        let player_1_moves = vec![2, 3, 5];
        let player_2_moves = vec![2, 2, 6];
        run_player_moves(&mut game, player_1_moves, player_2_moves);

        assert_eq!(game.winner, Player::None);
    }

    #[test]
    fn player_one_row_1_blocked() {
        let mut game = Game::default();
        let player_1_moves = vec![2, 3, 5];
        let player_2_moves = vec![2, 1, 4];
        run_player_moves(&mut game, player_1_moves, player_2_moves);

        assert_eq!(game.winner, Player::None);
    }

    #[test]
    fn player_one_row_1_win() {
        let mut game = Game::default();
        let player_1_moves = vec![3, 4, 6, 5];
        let player_2_moves = vec![3, 3, 6];
        run_player_moves(&mut game, player_1_moves, player_2_moves);

        assert_eq!(game.winner, Player::One);
    }

    #[test]
    fn player_one_column_3_one_short() {
        let mut game = Game::default();
        let player_1_moves = vec![3, 3, 3];
        let player_2_moves = vec![4, 6];
        run_player_moves(&mut game, player_1_moves, player_2_moves);

        assert_eq!(game.winner, Player::None);
    }

    #[test]
    fn player_one_column_3_win() {
        let mut game = Game::default();
        let player_1_moves = vec![3, 3, 3, 3];
        let player_2_moves = vec![4, 6, 5];
        run_player_moves(&mut game, player_1_moves, player_2_moves);

        assert_eq!(game.winner, Player::One);
    }

    #[test]
    fn player_two_column_3_win() {
        let mut game = Game::default();
        let player_1_moves = vec![4, 6, 5, 4];
        let player_2_moves = vec![3, 3, 3, 3];
        run_player_moves(&mut game, player_1_moves, player_2_moves);

        assert_eq!(game.winner, Player::Two)
    }

    fn run_player_moves(game: &mut Game, player_1_moves: Vec<usize>, player_2_moves: Vec<usize>) {
        let x: Result<(), MoveError> = to_moves(player_1_moves, player_2_moves)
            .iter()
            .try_fold((), |_acc, &m| game.play_move(m));

        match x {
            Ok(()) => (),
            e => assert_eq!("Ok(())", format!("{:?}", e)),
        }
    }

    fn to_moves(player_1_moves: Vec<usize>, player_2_moves: Vec<usize>) -> Vec<usize> {
        use itertools::interleave;
        interleave(player_1_moves.into_iter(), player_2_moves.into_iter()).collect::<Vec<_>>()
    }
}
