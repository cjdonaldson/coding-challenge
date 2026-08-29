use std::collections::HashSet;

use crate::board::{
    board_error::BoardError,
    cell_type::{CellType, Visibility},
    foreach_neighbor, Board,
};

/// uncovers (reveals) the patch under the selected cell
///
/// * `index` the linear index into the board
pub fn reveal_cell_by_index(board: &mut Board, index: usize) -> Result<CellType, BoardError> {
    if index >= board.properties.cell_count() {
        Err(BoardError::CellOutOfBounds)
    } else {
        board.cells[index] = match board.cells[index] {
            CellType::Bomb(_visibility) => {
                println!("you lose (x)");
                CellType::Bomb(Visibility::Visible)
            }
            CellType::Safe(_visibility) => {
                // more recurse
                CellType::Safe(Visibility::Visible)
            }
            CellType::Warn(count, _visibility) => CellType::Warn(count, Visibility::Visible),
        };
        match board.cells[index] {
            CellType::Safe(_) => {
                println!("this one");
                reveal_neighbors_of(board, index)
            }
            _cell_type => (),
            // (e) => panic!("{}", e),
        }

        Ok(board.cells[index])
    }
}

fn reveal_neighbors_of(board: &mut Board, index: usize) {
    // let mut others = false;
    let reveal_neighbor_cell = |c: CellType| -> CellType {
        match c {
            CellType::Bomb(visibility) => {
                // board.lost
                println!("oops should not be here");
                CellType::Bomb(visibility)
            }
            CellType::Safe(_visibility) => {
                println!("reveal more");

                // others =
                CellType::Safe(Visibility::Visible)
            }
            CellType::Warn(c, _visibility) => CellType::Warn(c, Visibility::Visible),
        }
    };

    println!("cjd -> {} {}", index, board.cells[index]);
    foreach_neighbor(board, index, reveal_neighbor_cell);
}

/// uncovers (reveals) the patch under the selected cell and neighbors if a safe patch
///
/// * `row` the row index into the board
/// * `column` the column index into the board
fn reveal_cell_by_rc(board: &mut Board, row: usize, column: usize) -> Result<CellType, BoardError> {
    let index = board.properties.index_from_rc(row, column)?;

    let ct = reveal_cell_by_index(board, index);
    if let Ok(CellType::Safe(_)) = ct {
        let mut visited_cells: HashSet<usize> = HashSet::new();
        visited_cells.insert(index);

        let mut safe_neighbors: Vec<usize> = Vec::new();
        // neighbors can be extracted to this file and removed from board
        if let Some(n) = board.neighbors.get(&index) {
            // TODO: can this be x.extend(board.neighbors.....into_iter());
            safe_neighbors.extend(n.iter());
        };

        while let Some(neighbor_index) = safe_neighbors.pop() {
            if visited_cells.contains(&neighbor_index) {
                continue;
            } else if let Ok(CellType::Safe(_)) = reveal_cell_by_index(board, neighbor_index) {
                visited_cells.insert(neighbor_index);
                if let Some(n) = board.neighbors.get(&neighbor_index) {
                    safe_neighbors.extend(n.iter());
                };
            }
        }
    }

    ct
}

/// uncovers (reveals) the cell under the selected cell
///
/// * `selection` the players selection; ie: a2
fn reveal_selection(board: &mut Board, selection: &str) -> Result<CellType, BoardError> {
    let (row, column) = board.properties.parse(selection)?;

    reveal_cell_by_rc(board, row, column)
}

pub fn make_move(selection: &str, board: &mut Board) -> Result<CellType, BoardError> {
    reveal_selection(board, selection)
}
