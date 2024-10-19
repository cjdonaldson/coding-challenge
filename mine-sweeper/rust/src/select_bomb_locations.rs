use crate::board::board_properties::BoardProperties;

use rand::distributions;
use rand::Rng;

use std::collections::HashSet;

pub fn select_bomb_locations(
    board: BoardProperties,
    select_count: usize,
) -> HashSet<(usize, usize)> {
    let cell_count = board.cell_count();
    assert!(
        select_count < cell_count,
        "expecting {select_count} to be less than {cell_count} available cells in the grid",
    );

    fn fill_set(cell_count: usize, target_count: usize) -> HashSet<usize> {
        let mut rnd_itr: distributions::DistIter<
            distributions::Uniform<usize>,
            rand::prelude::ThreadRng,
            usize,
        > = rand::thread_rng().sample_iter(distributions::Uniform::new(0, cell_count));

        let mut locations: HashSet<usize> = HashSet::with_capacity(cell_count);

        while locations.len() < target_count {
            let r = rnd_itr.next().unwrap();
            locations.insert(r);
        }
        locations
    }

    let flipped = select_count > cell_count / 2;

    let target_count = if flipped {
        cell_count - select_count
    } else {
        select_count
    };

    let locations = fill_set(cell_count, target_count);

    let resolved_locations = if flipped {
        let all_locations: HashSet<usize> =
            HashSet::from_iter((0usize..board.cell_count()).into_iter());

        all_locations.difference(&locations).cloned().collect()
    } else {
        locations
    };

    let cell_locations: HashSet<(usize, usize)> = resolved_locations
        .iter()
        .map(|x| (x / board.columns, x % board.columns))
        .collect();

    cell_locations
}

// cargo test -- --nocapture # there were once println's in the test
#[cfg(test)]
mod tests {
    use super::*;
    use crate::board_properties::Difficulty;
    use ntest::*;

    static TEST_BOARD: BoardProperties = BoardProperties::new(6, 6, Difficulty::Easy);

    #[test]
    #[should_panic]
    fn select_bomb_locations_panic_test() {
        let max_count = TEST_BOARD.cell_count() + 1;
        select_bomb_locations(TEST_BOARD, max_count);
    }

    #[test]
    #[timeout(3)]
    fn select_bomb_locations_test() {
        let max_count = TEST_BOARD.cell_count();
        for count in 1..max_count {
            let res = select_bomb_locations(TEST_BOARD, count).len();
            assert_eq!(count, res);
        }
    }
}
