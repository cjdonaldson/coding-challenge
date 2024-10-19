use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum BoardError {
    CellOutOfBounds,
    // CellInvalid(usize, usize),
    CellParseError { message: String },
}

impl Display for BoardError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CellOutOfBounds => write!(f, "CellOutOfBounds"),
            // Self::CellInvalid(r, c) => write!(f, "CellInvalid({r},{c})"),
            Self::CellParseError { message } => write!(f, "CellParseError: {message}"),
        }
    }
}
