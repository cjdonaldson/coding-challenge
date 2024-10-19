use crate::board::board_properties::Difficulty;

pub fn bomb_count(cell_count: usize, difficulty: Difficulty) -> usize {
    match difficulty {
        Difficulty::Easy => cell_count / 6,
        Difficulty::Medium => cell_count / 4,
        Difficulty::Hard => cell_count / 3,
    }
}

#[test]
fn bomb_count_test() {
    assert_eq!(6, bomb_count(36, Difficulty::Easy));
    assert_eq!(9, bomb_count(36, Difficulty::Medium));
    assert_eq!(12, bomb_count(36, Difficulty::Hard));
}
