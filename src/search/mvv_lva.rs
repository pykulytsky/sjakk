use crate::{board::Board, moves::MoveList, Move};

const MVV_LVA: [[isize; 6]; 6] = [
    [1, -4, -2, -3, -4, 1],
    [14, 11, 13, 12, -11, 12],
    [24, -21, 21, 21, -22, 23],
    [24, -21, 21, 21, -22, 23],
    [35, 33, 34, 34, 31, 35],
    [66, 65, 64, 63, 62, 61],
];

#[inline]
pub fn score_moves(move_list: MoveList, board: &Board, tt_move: Option<Move>) -> MoveList {
    let mut list = move_list
        .iter()
        .map(|m| {
            let piece = board.moved_piece(m);
            let capture = board.captured_piece(m);
            let mut score = 0;
            if let Some(tt_m) = tt_move {
                if *m == tt_m {
                    score = isize::MAX;
                }
            } else if m.is_promotion() {
                score = MVV_LVA[m.promotion_to().unwrap() as usize][piece as usize] + 1;
            } else if let Some(captured_piece) = capture {
                score = MVV_LVA[captured_piece as usize][piece as usize];
            }
            (score, m.to_owned())
        })
        .collect::<Vec<(isize, Move)>>();

    list.sort_by(|(score, _), (score_b, _)| score_b.cmp(score));
    list.into_iter().map(|(_, m)| m).collect::<MoveList>()
}
