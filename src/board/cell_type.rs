use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
    Hidden,
    Visible,
}

#[derive(Debug, Clone, Copy)]
pub enum CellType {
    // enum struct
    // Bomb { visibility: Visibility },
    // Safe { visibility: Visibility },
    // enum tuple
    Bomb(Visibility),
    Safe(Visibility),
    Warn(usize, Visibility),
}

impl fmt::Display for CellType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            CellType::Bomb(visibility) => {
                if visibility == Visibility::Visible {
                    write!(f, " B ")
                } else {
                    write!(f, " ? ")
                }
            }
            CellType::Safe(visibility) => {
                if visibility == Visibility::Visible {
                    write!(f, "   ")
                } else {
                    write!(f, " ? ")
                }
            }
            CellType::Warn(count, visibility) => {
                if visibility == Visibility::Visible {
                    write!(f, " {count} ")
                } else {
                    write!(f, " ? ")
                }
            }
        }
    }
}
