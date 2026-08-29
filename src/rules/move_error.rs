#[derive(Debug)]
pub struct PlayerInput(pub String);

#[derive(Debug)]
pub struct ErrorMessage(pub String);

#[derive(Debug)]
pub enum MoveError {
    GameFinished,
    InvalidColumn(String),
    ColumnFull,
    ParseError(ErrorMessage, PlayerInput), // TODO: type aliases
}

impl std::fmt::Display for MoveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MoveError::ColumnFull => write!(f, "column is full"),
            MoveError::InvalidColumn(i) => write!(f, "column must be between 1 and 7: given {i}"),
            MoveError::GameFinished => write!(f, "game is already finished"),
            MoveError::ParseError(ErrorMessage(s), PlayerInput(inp)) => {
                write!(f, "{} '{}': column must be between 1 and 7", s, inp.trim())
            }
        }
    }
}
