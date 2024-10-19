use std::fmt;

use super::board_error::BoardError;

#[derive(Debug, Clone, Copy)]
pub enum Difficulty {
    Easy,
    Medium,
    Hard,
}

impl fmt::Display for Difficulty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Difficulty::Easy => write!(f, "Easy"),
            Difficulty::Medium => write!(f, "Medium"),
            Difficulty::Hard => write!(f, "Hard"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoardProperties {
    pub rows: usize,
    pub columns: usize,
    pub difficulty: Difficulty,
}

fn is_alpha(c: &char) -> bool {
    c >= &'A' && c <= &'Z'
}

impl BoardProperties {
    pub const fn new(rows: usize, columns: usize, difficulty: Difficulty) -> Self {
        Self {
            rows,
            columns,
            difficulty,
        }
    }

    pub fn cell_count(&self) -> usize {
        self.rows * self.columns
    }

    fn parse_column(self, s: &str) -> Result<usize, BoardError> {
        let column_part: Vec<usize> = s
            .to_ascii_uppercase()
            .chars()
            .take_while(is_alpha)
            .map(|c| c as usize - 65)
            .collect();

        let column = column_part
            .iter()
            .fold(0usize, |acc, c| acc * self.columns + c);

        if column_part.is_empty() {
            Err(BoardError::CellParseError {
                message: format!("invalid column {:?} from {s}", column_part),
            })
        } else if column > self.columns {
            Err(BoardError::CellParseError {
                message: format!("invalid column {column} from {s}"),
            })
        } else {
            Ok(column)
        }
    }

    fn parse_row(self, s: &str) -> Result<usize, BoardError> {
        let row_part = s
            .to_ascii_uppercase()
            .chars()
            .skip_while(is_alpha)
            .collect::<String>();

        let row = row_part.parse::<usize>();

        match row {
            Ok(r) if r < self.rows => Ok(r),
            Err(_) => Err(BoardError::CellParseError {
                message: format!("invalid row from {s}"),
            }),
            Ok(_) => Err(BoardError::CellParseError {
                message: format!("invalid row {row_part} from {s}"),
            }),
        }
    }

    /// parse string slice into (row, column)
    ///
    /// * `s` players input to convert; ie: b2
    pub fn parse(self, s: &str) -> Result<(usize, usize), BoardError> {
        let column = self.parse_column(s);
        let row = self.parse_row(s);

        match (row, column) {
            (Ok(r), Ok(c)) => Ok((r, c)),
            (Ok(r), Err(m)) => Err(BoardError::CellParseError {
                message: format!("Cell({r}, ?): {m}"),
            }),
            (Err(m), Ok(c)) => Err(BoardError::CellParseError {
                message: format!("Cell(?, {c}): {m}"),
            }),
            (Err(_), Err(_)) => Err(BoardError::CellParseError {
                message: format!("Cell(?, ?): CellParseError: invalid row and column from {s}"),
            }),
        }
    }

    /// convert and index in to a (row, col) equivelant or err
    ///
    /// * `index` the index to attempt a convert to (row, column)
    fn rc_from_index(&self, index: usize) -> Result<(usize, usize), BoardError> {
        // borrow self to avoid move semantic related issues
        if index > self.cell_count() {
            Err(BoardError::CellOutOfBounds)
        } else {
            let rc: (usize, usize) = (index / self.columns, index % self.columns);
            Ok(rc)
        }
    }

    /// validates and derives the indicated index into the matrix
    ///
    /// * `index` the index base to apply adjustments
    /// * `row_offset` the number of row(s) to move
    /// * `column_offset` the number of column(s) to move
    pub fn new_index(
        &self, // borrow self to avoid move semantic related issues
        index: usize,
        row_offset: isize,
        column_offset: isize,
    ) -> Result<usize, BoardError> {
        // the `?` operator reduces Result pattern matching processing
        let (r, c) = self.rc_from_index(index)?;
        let new_r = r as isize + row_offset;
        let new_c = c as isize + column_offset;
        if new_r < 0 || new_c < 0 {
            Err(BoardError::CellOutOfBounds)
        } else if new_r >= self.rows as isize {
            Err(BoardError::CellOutOfBounds)
        } else if new_c >= self.columns as isize {
            Err(BoardError::CellOutOfBounds)
        } else {
            let new_index =
                index as isize + row_offset * self.columns as isize + column_offset as isize;
            // self.validate_rc(row, column)
            Ok(new_index as usize)
        }
    }

    /// converts row and column into a valid index
    ///
    /// * `row` the row index into the board
    /// * `column` the column index into the board
    pub fn index_from_rc(&self, row: usize, column: usize) -> Result<usize, BoardError> {
        self.new_index(0, row as isize, column as isize)
    }
}

#[test]
fn in_grid_parse_test() {
    let props = BoardProperties::new(6, 6, Difficulty::Easy);
    assert_eq!(Ok((0, 0)), props.parse("a0"));
    assert_eq!(Ok((4, 2)), props.parse("c4"));
    assert_eq!(Ok((5, 5)), props.parse("F5"));
}

#[test]
fn error_parse_test() {
    let props = BoardProperties::new(6, 6, Difficulty::Easy);
    assert_eq!(
        Err(BoardError::CellParseError {
            message: String::from("Cell(?, 0): CellParseError: invalid row from a-1")
        }),
        props.parse("a-1")
    );
    assert_eq!(
        Err(BoardError::CellParseError {
            message: String::from("Cell(0, ?): CellParseError: invalid column 25 from Z0")
        }),
        props.parse("Z0")
    );
    assert_eq!(
        Err(BoardError::CellParseError {
            message: String::from("Cell(?, ?): CellParseError: invalid row and column from Z9")
        }),
        props.parse("Z9")
    );
}
