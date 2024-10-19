use super::cell::Cell;
use super::cell_error::CellError;
use super::direction::Direction;

pub struct BoundedCell {
    max_row: usize,
    max_column: usize,
}

impl BoundedCell {
    pub fn new(max_row: usize, max_column: usize) -> BoundedCell {
        BoundedCell {
            max_row,
            max_column,
        }
    }

    pub fn cell_from(&self, row: isize, col: isize) -> Result<Cell, CellError> {
        if row < 0 || (row as usize) >= self.max_row {
            Err(CellError::InvalidRow(row))
        } else if col < 0 || (col as usize) >= self.max_column {
            Err(CellError::InvalidColumn(col))
        } else {
            Ok(Cell {
                row: row as usize,
                col: col as usize,
            })
        }
    }

    pub fn cell_from_direction(
        &self,
        cell: &Cell,
        direction: &Direction,
    ) -> Result<Cell, CellError> {
        let r: isize = cell.row as isize;
        let c: isize = cell.col as isize;

        let (rr, cc) = match direction {
            Direction::Horizontal => (r, c + 1),
            Direction::Vertical => (r + 1, c),
            Direction::Major => (r + 1, c + 1),
            Direction::Minor => (r + 1, c - 1),
        };

        self.cell_from(rr, cc)
    }
}
