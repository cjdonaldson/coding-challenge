#[derive(Clone, Copy, PartialEq)]
pub struct Cell {
    pub row: usize,
    pub col: usize,
}

impl std::fmt::Display for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(r{}, c{})", self.row + 1, self.col + 1)
    }
}
