use std::mem::transmute;

use strum_macros::EnumIter;

use crate::{
    constants::{CLEAR_FILE, MASK_RANK},
    rays::RAY_ATTACKS,
    utils::{bit_scan_forward, bit_scan_reverse, POSITIVE_RAYS},
    Bitboard, Square,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn opposite(&self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

impl ToString for Color {
    fn to_string(&self) -> String {
        match self {
            Color::White => "w".to_string(),
            Color::Black => "b".to_string(),
        }
    }
}

/// Represents all possible pieces in chess, can be used in [`Board`] or [`Position`] to index
/// required bitboard by piece type.
#[derive(Debug, EnumIter, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl PieceType {
    /// Generates pseudo legal moves, for given piece.
    pub fn pseudo_legal_moves(
        &self,
        square: Square,
        color: Color,
        occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        match self {
            PieceType::Pawn => Pawn::pseudo_legal_moves(square, color, occupied, own),
            PieceType::Rook => Rook::pseudo_legal_moves(square, color, occupied, own),
            PieceType::Knight => Knight::pseudo_legal_moves(square, color, occupied, own),
            PieceType::Bishop => Bishop::pseudo_legal_moves(square, color, occupied, own),
            PieceType::Queen => Queen::pseudo_legal_moves(square, color, occupied, own),
            PieceType::King => King::pseudo_legal_moves(square, color, occupied, own),
        }
    }

    #[inline]
    pub fn from_index(i: usize) -> Self {
        unsafe { transmute((i as u8) & 7) }
    }

    pub fn unicode(&self, color: Color) -> &str {
        match (color, self) {
            (Color::White, PieceType::Pawn) => "♙",
            (Color::White, PieceType::Rook) => "♖",
            (Color::White, PieceType::Knight) => "♘",
            (Color::White, PieceType::Bishop) => "♗",
            (Color::White, PieceType::Queen) => "♕",
            (Color::White, PieceType::King) => "♔",
            (Color::Black, PieceType::Pawn) => "♟︎",
            (Color::Black, PieceType::Rook) => "♜",
            (Color::Black, PieceType::Knight) => "♞",
            (Color::Black, PieceType::Bishop) => "♝",
            (Color::Black, PieceType::Queen) => "♛",
            (Color::Black, PieceType::King) => "♚",
        }
    }
}

impl std::fmt::Display for PieceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let notation = match self {
            PieceType::Pawn => "p",
            PieceType::Rook => "r",
            PieceType::Knight => "n",
            PieceType::Bishop => "b",
            PieceType::Queen => "q",
            PieceType::King => "k",
        };

        write!(f, "{notation}")
    }
}

pub trait Piece {
    /// Generates pseudo legal moves.
    /// The main difference betweeen legal and pseudo-legal move is that pseudo-legal moves don't
    /// take into consideration checks an pins on the board. To generate legal moves use
    /// [`Board::legal_moves`].
    /// In case of pawns, this method generates available moves and attacks, but don't generate
    /// en-passant, since we need to know previous move.
    /// (https://www.chessprogramming.org/Pseudo-Legal_Move)[More information about pseudo-legal
    /// moves]
    fn pseudo_legal_moves(
        square: Square,
        color: Color,
        occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard;
}

pub struct Pawn;

impl Piece for Pawn {
    fn pseudo_legal_moves(
        square: Square,
        color: Color,
        occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        let sq = square.0;
        let sq = Bitboard::from_square_number(sq);
        let one_step = match color {
            Color::White => (sq << 8) & !occupied.0,
            Color::Black => (sq >> 8) & !occupied.0,
        };
        let two_steps = match color {
            Color::White => ((one_step & MASK_RANK[2]) << 8) & !occupied.0,
            Color::Black => ((one_step & MASK_RANK[5]) >> 8) & !occupied.0,
        };
        let valid_moves = one_step | two_steps;
        let pawn_attacks = Self::pawn_attacks(color, square);
        let enemy = occupied ^ own;
        let valid_attacks = pawn_attacks & enemy.0;

        valid_moves | valid_attacks
    }
}

impl Pawn {
    pub fn pawn_attacks(color: Color, sq: Square) -> Bitboard {
        let sq = Bitboard::from_square(sq);
        let (left_attack, right_attack) = match color {
            Color::White => ((sq & CLEAR_FILE[0]) << 7, (sq & CLEAR_FILE[7]) << 9),
            Color::Black => ((sq & CLEAR_FILE[7]) >> 7, (sq & CLEAR_FILE[0]) >> 9),
        };
        left_attack | right_attack
    }
}

/// Generates pseudo-legal moves for sliding pieces (Rook, Bishop and Queen).
fn slideing_piece_pseudo_legal_moves(
    sq: usize,
    occupied: Bitboard,
    own: Bitboard,
    diag_attacks: usize,
) -> Bitboard {
    let mut sliding_attacks = 0;

    for i in 0..4 {
        let i = i * 2 + diag_attacks;
        let mut attacks = RAY_ATTACKS[sq][i];
        let blocker = attacks & occupied.0;
        if blocker != 0 {
            let blocker_square = if POSITIVE_RAYS.contains(&i) {
                bit_scan_forward(blocker)
            } else {
                bit_scan_reverse(blocker)
            } as usize;
            attacks ^= RAY_ATTACKS[blocker_square][i];
        }

        sliding_attacks |= attacks;
    }

    Bitboard((sliding_attacks ^ own.0) & sliding_attacks)
}

pub struct Rook;

impl Piece for Rook {
    fn pseudo_legal_moves(
        square: Square,
        _color: Color,
        occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        let sq = square.0 as usize;
        slideing_piece_pseudo_legal_moves(sq, occupied, own, 0)
    }
}

pub struct Bishop;

impl Piece for Bishop {
    fn pseudo_legal_moves(
        square: Square,
        _color: Color,
        occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        let sq = square.0 as usize;
        slideing_piece_pseudo_legal_moves(sq, occupied, own, 1)
    }
}

pub struct Queen;

impl Piece for Queen {
    fn pseudo_legal_moves(
        square: Square,
        _color: Color,
        occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        let sq = square.0 as usize;
        slideing_piece_pseudo_legal_moves(sq, occupied, own, 0)
            | slideing_piece_pseudo_legal_moves(sq, occupied, own, 1)
    }
}

pub struct Knight;

impl Piece for Knight {
    fn pseudo_legal_moves(
        square: Square,
        _color: Color,
        _occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        let src = Bitboard::from_square(square);
        let spot1_clip = CLEAR_FILE[0] & CLEAR_FILE[1];
        let spot2_clip = CLEAR_FILE[0];
        let spot3_clip = CLEAR_FILE[7];
        let spot4_clip = CLEAR_FILE[7] & CLEAR_FILE[6];
        let spot5_clip = CLEAR_FILE[7] & CLEAR_FILE[6];
        let spot6_clip = CLEAR_FILE[7];
        let spot7_clip = CLEAR_FILE[0];
        let spot8_clip = CLEAR_FILE[0] & CLEAR_FILE[1];

        let spot_1 = (src.0 & spot1_clip) << 6;
        let spot_2 = (src.0 & spot2_clip) << 15;
        let spot_3 = (src.0 & spot3_clip) << 17;
        let spot_4 = (src.0 & spot4_clip) << 10;

        let spot_5 = (src.0 & spot5_clip) >> 6;
        let spot_6 = (src.0 & spot6_clip) >> 15;
        let spot_7 = (src.0 & spot7_clip) >> 17;
        let spot_8 = (src.0 & spot8_clip) >> 10;

        let valid_moves = spot_1 | spot_2 | spot_3 | spot_4 | spot_5 | spot_6 | spot_7 | spot_8;
        let valid_moves = valid_moves & !own.0;

        Bitboard(valid_moves)
    }
}

pub struct King;

impl Piece for King {
    fn pseudo_legal_moves(
        square: Square,
        _color: Color,
        _occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        let src = Bitboard::from_square(square);
        let king_clip_file_h = src.0 & CLEAR_FILE[7];
        let king_clip_file_a = src.0 & CLEAR_FILE[0];

        let spot_1 = king_clip_file_a << 7;
        let spot_2 = src.0 << 8;
        let spot_3 = king_clip_file_h << 9;
        let spot_4 = king_clip_file_h << 1;

        let spot_5 = king_clip_file_h >> 7;
        let spot_6 = src.0 >> 8;
        let spot_7 = king_clip_file_a >> 9;
        let spot_8 = king_clip_file_a >> 1;

        let valid_moves = spot_1 | spot_2 | spot_3 | spot_4 | spot_5 | spot_6 | spot_7 | spot_8;
        let valid_moves = valid_moves & !own.0;

        Bitboard(valid_moves)
    }
}

#[cfg(test)]
mod tests {
    use crate::constants;

    use super::*;

    #[test]
    fn pawn_pseudo_legal_moves() {
        let moves = Pawn::pseudo_legal_moves(
            Square(9),
            Color::White,
            Bitboard(constants::ALL_PIECES),
            Bitboard(constants::WHITE_PIECES),
        );

        let available_squares: Vec<Square> = moves.into_iter().collect();
        assert_eq!(available_squares.len(), 2);
        assert_eq!(available_squares[0].0, 17);
        assert_eq!(available_squares[1].0, 25);

        let moves = Pawn::pseudo_legal_moves(
            Square(35),
            Color::White,
            Bitboard(constants::ALL_PIECES),
            Bitboard(constants::WHITE_PIECES),
        );

        let available_squares: Vec<Square> = moves.into_iter().collect();
        assert_eq!(available_squares.len(), 1);
        assert_eq!(available_squares[0].0, 43);
    }

    #[test]
    fn pawn_attacks() {
        let occupied = Bitboard::from_square(Square(44));
        let moves = Pawn::pseudo_legal_moves(Square(35), Color::White, occupied, Bitboard(0));

        let available_squares: Vec<Square> = moves.into_iter().collect();

        assert_eq!(available_squares.len(), 2);
        assert_eq!(available_squares[0].0, 43);
        assert_eq!(available_squares[1].0, 44);
    }

    #[test]
    fn pawn_blocked() {
        let occupied = Bitboard::from_square(Square(43));
        let moves = Pawn::pseudo_legal_moves(Square(35), Color::White, occupied, Bitboard(0));

        assert_eq!(moves.into_iter().count(), 0);
    }

    #[test]
    fn sliding_attacks() {
        let moves = slideing_piece_pseudo_legal_moves(28, Bitboard(0), Bitboard(0), 0);
        assert_eq!(moves.0.count_ones(), 14);
    }

    #[test]
    fn sliding_attacks_on_diags() {
        let moves = slideing_piece_pseudo_legal_moves(28, Bitboard(0), Bitboard(0), 1);
        assert_eq!(moves.0.count_ones(), 13);
    }

    #[test]
    fn sliding_attacks_with_friendly_blockers() {
        let north_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::North);
        let south_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::South);
        let west_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::West);
        let east_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::East);
        let own = north_blocker | south_blocker | west_blocker | east_blocker;
        let moves = slideing_piece_pseudo_legal_moves(28, own, own, 0);
        assert_eq!(moves.0.count_ones(), 0);
    }

    #[test]
    fn sliding_attacks_with_enemy_blockers() {
        let north_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::North);
        let south_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::South);
        let west_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::West);
        let east_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::East);

        let enemy = north_blocker | south_blocker | west_blocker | east_blocker;
        let moves = slideing_piece_pseudo_legal_moves(28, enemy, Bitboard(0), 0);
        assert_eq!(moves.0.count_ones(), 4);
    }

    #[test]
    fn sliding_attacks_with_friendly_blockers_on_diags() {
        let north_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::NorthEast);
        let south_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::SouthEast);
        let west_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::NorthWest);
        let east_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::SouthWest);
        let own = north_blocker | south_blocker | west_blocker | east_blocker;
        let moves = slideing_piece_pseudo_legal_moves(28, own, own, 1);
        assert_eq!(moves.0.count_ones(), 0);
    }

    #[test]
    fn sliding_attacks_with_enemy_blockers_on_diags() {
        let north_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::NorthEast);
        let south_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::SouthEast);
        let west_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::NorthWest);
        let east_blocker =
            Bitboard::from_square_number(28).one_step_by_direction(crate::Direction::SouthWest);

        let enemy = north_blocker | south_blocker | west_blocker | east_blocker;
        let moves = slideing_piece_pseudo_legal_moves(28, enemy, Bitboard(0), 1);
        assert_eq!(moves.0.count_ones(), 4);
    }

    #[test]
    fn knight_pseudo_legal_moves() {
        let moves_on_empty_board =
            Knight::pseudo_legal_moves(Square(28), Color::White, Bitboard(0), Bitboard(0));
        assert_eq!(moves_on_empty_board.0.count_ones(), 8);

        let moves_on_occupied_board = Knight::pseudo_legal_moves(
            Square(28),
            Color::White,
            moves_on_empty_board,
            moves_on_empty_board,
        );
        assert_eq!(moves_on_occupied_board.0.count_ones(), 0);
    }
}
