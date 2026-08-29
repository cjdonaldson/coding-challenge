use super::BOARD_HEIGHT;
use super::BOARD_WIDTH;

#[derive(Debug)]
pub enum CellError {
    InvalidColumn(isize),
    InvalidRow(isize),
}

impl std::fmt::Display for CellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CellError::InvalidRow(i) => {
                write!(f, "row must be between 1 and {BOARD_HEIGHT}: given {i}")
            }
            CellError::InvalidColumn(i) => {
                write!(f, "column must be between 1 and {BOARD_WIDTH}: given {i}")
            }
        }
    }
}
