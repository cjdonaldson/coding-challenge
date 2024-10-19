pub mod board_error;
pub mod board_properties;
pub mod cell_type;

use std::collections::HashMap;
// use std::fmt;
use std::time::SystemTime;
// use time_hms::TimeHms;

use board_error::BoardError;
use board_properties::{BoardProperties, Difficulty};
use cell_type::{CellType, Visibility};

// passed in some how?
// so that this is mostly a container
use crate::bomb_count::bomb_count;
use crate::select_bomb_locations::select_bomb_locations;

/// apply fn(CellType) -> CellType to applicable neighboring cells
/// - foreach is a mutating (side-effecting) functional operations
///
/// * `index` the center cell of the neighbors to check
/// * `fn(CellType)` applied to valid neighbors
pub fn foreach_neighbor(board: &mut Board, index: usize, f: fn(CellType) -> CellType) -> () {
    // TODO: can this be an iterator
    if let Some(indices) = board.neighbors.get(&index) {
        for n_index in indices {
            board.cells[*n_index] = f(board.cells[*n_index]);
        }
    }
}

pub struct Board {
    pub properties: BoardProperties,
    pub cells: Vec<CellType>,
    pub bomb_count: usize,
    marked: usize,
    start_time: SystemTime,
    pub neighbors: HashMap<usize, Vec<usize>>,
}

impl Board {
    pub fn new(rows: usize, columns: usize, difficulty: Difficulty) -> Self {
        let properties = BoardProperties::new(rows, columns, difficulty);
        let bomb_count = bomb_count(properties.cell_count(), difficulty);

        let mut board = Self {
            properties,
            cells: [CellType::Safe(Visibility::Hidden)].repeat(rows * columns),
            bomb_count,
            marked: 0,
            start_time: SystemTime::now(),
            neighbors: HashMap::new(),
        };

        let offsets: Vec<(isize, isize)> = vec![
            (-1, -1), // up left
            (-1, 0),  // up
            (-1, 1),  // up right
            (0, -1),  //  left
            (0, 1),   // right
            (1, -1),  // down left
            (1, 0),   // down
            (1, 1),   // down right
        ];
        // derive cell neighbors for faster neighbor operations
        for index in 0..properties.cell_count() {
            let mut n: Vec<usize> = Vec::new();
            for (r, c) in &offsets {
                if let Ok(i) = properties.new_index(index, *r, *c) {
                    n.push(i);
                }
            }
            board.neighbors.insert(index, n);
        }

        // let locations = select_bomb_locations(properties, bomb_count);
        // for (r, c) in locations {
        for (r, c) in select_bomb_locations(properties, bomb_count) {
            let _ = board.place_bomb(r, c);
        }

        board
    }

    ///  places the bomb if given valid row and column
    ///  and adjusts the number counts in adjacent cells
    ///
    /// * `row` target row placement
    /// * `column` target column placement
    pub fn place_bomb(&mut self, row: usize, column: usize) -> Result<(), BoardError> {
        let index = self
            .properties
            .new_index(0, row as isize, column as isize)?;

        self.cells[index] = CellType::Bomb(Visibility::Hidden);

        let adjust_neighbor_counts = |c: CellType| -> CellType {
            match c {
                CellType::Bomb(_visibility) => CellType::Bomb(Visibility::Hidden),
                CellType::Safe(_visibility) => CellType::Warn(1, Visibility::Hidden),
                CellType::Warn(c, _visibility) => CellType::Warn(c + 1, Visibility::Hidden),
            }
        };

        foreach_neighbor(self, index, adjust_neighbor_counts);
        Ok(())
    }
}

// fn clear_terminal() -> () {
//     print!("{esc}[2J{esc}[1;1H", esc = 27u8 as char);
// }
//
// impl fmt::Display for Board {
//     fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let elapsed = match self.start_time.elapsed() {
//             Ok(x) => TimeHms::new(x.as_secs()),
//             Err(_) => TimeHms::new(0), // TODO: make this infailable
//         };
//
//         let stats = || {
//             println!(
//                 "{}    {}   {}",
//                 elapsed,
//                 self.properties.difficulty,
//                 self.bomb_count - self.marked
//             );
//         };
//
//         let column_line = || {
//             print!(" ");
//             for c in 0..self.properties.columns {
//                 print!("  {}", (65u8 + c as u8) as char);
//             }
//             println!("");
//         };
//
//         let row_line = || {
//             for r in 0..self.properties.rows {
//                 print!("{r} ");
//                 for c in 0..self.properties.columns {
//                     print!("{}", self.cells[r * self.properties.columns + c]);
//                 }
//                 println!(" {r}");
//             }
//         };
//
//         clear_terminal();
//         stats();
//         column_line();
//         row_line();
//         column_line();
//
//         Ok(())
//     }
// }
