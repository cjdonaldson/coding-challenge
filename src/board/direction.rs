pub enum Direction {
    Horizontal,
    Vertical,
    Major,
    Minor,
}

impl Direction {
    pub const ALL: [Direction; 4] = [
        Direction::Horizontal,
        Direction::Vertical,
        Direction::Major,
        Direction::Minor,
    ];
}
