use std::mem::transmute;

use crate::{
    board::Board,
    constants::CLEAR_FILE,
    gen_moves::{
        get_bishop_moves, get_king_moves, get_knight_moves, get_pawn_moves, get_rook_moves,
    },
    moves::MoveList,
    rays::RAY_ATTACKS,
    utils::{bit_scan_forward, bit_scan_reverse, POSITIVE_RAYS},
    Bitboard, Move, Rank, Square,
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

    pub fn eval_mask(&self) -> i32 {
        match self {
            Color::White => 1,
            Color::Black => -1,
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl PieceType {
    pub const ALL: [Self; 6] = [
        Self::Pawn,
        Self::Rook,
        Self::Knight,
        Self::Bishop,
        Self::Queen,
        Self::King,
    ];

    pub const PAWN: usize = 0;
    pub const ROOK: usize = 1;
    pub const KNIGHT: usize = 2;
    pub const BISHOP: usize = 3;
    pub const QUEEN: usize = 4;
    pub const KING: usize = 5;

    #[inline]
    pub fn value(&self) -> u32 {
        match self {
            PieceType::Pawn => 1,
            PieceType::Rook => 5,
            PieceType::Knight => 3,
            PieceType::Bishop => 3,
            PieceType::Queen => 9,
            PieceType::King => 255,
        }
    }

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

    fn legal_moves(
        board: &Board,
        checkers: Bitboard,
        pinned: Bitboard,
        pieces: Bitboard,
        own_pieces: Bitboard,
        move_list: &mut MoveList,
        check_mask: Bitboard,
    ) {
        for sq in pieces {
            let mut bb =
                Self::pseudo_legal_moves(sq, board.side_to_move, board.all_pieces(), own_pieces);
            if bb == 0 {
                continue;
            }
            if checkers.0.count_ones() == 1 {
                bb &= check_mask;
            }

            let pinned = pinned & Bitboard::from_square(sq) != 0;
            if pinned {
                let king_square = board.ksq;
                let pin = board.get_ray(king_square, sq);
                bb &= pin;
            }
            for target in bb {
                move_list.push(Move::new(sq, target));
            }
        }
    }
}

pub struct Pawn;

impl Piece for Pawn {
    fn pseudo_legal_moves(
        square: Square,
        color: Color,
        occupied: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        get_pawn_moves(square, color, occupied) & !own
    }

    fn legal_moves(
        board: &Board,
        checkers: Bitboard,
        pinned: Bitboard,
        pieces: Bitboard,
        own_pieces: Bitboard,
        move_list: &mut MoveList,
        check_mask: Bitboard,
    ) {
        for sq in pieces {
            let mut bb =
                Pawn::pseudo_legal_moves(sq, board.side_to_move, board.all_pieces(), own_pieces);
            if bb == 0 {
                continue;
            }
            if checkers.0.count_ones() == 1 {
                bb &= check_mask;
            }

            let pinned = pinned & Bitboard::from_square(sq) != 0;
            if pinned {
                let king_square = board.ksq;
                let pin = board.get_ray(king_square, sq);
                bb &= pin;
            }
            for target in bb {
                if (target.rank() == Rank::Rank8 && board.side_to_move == Color::White)
                    || (target.rank() == Rank::Rank1 && board.side_to_move == Color::Black)
                {
                    move_list.push(Move::new_promotion(sq, target, PieceType::Queen));
                    move_list.push(Move::new_promotion(sq, target, PieceType::Rook));
                    move_list.push(Move::new_promotion(sq, target, PieceType::Bishop));
                    move_list.push(Move::new_promotion(sq, target, PieceType::Knight));
                } else {
                    move_list.push(Move::new(sq, target));
                }
            }
        }
    }
}

impl Pawn {
    pub fn pawn_attacks(color: Color, sq: Square) -> Bitboard {
        // get_pawn_attacks(sq, color, Bitboard(0))
        let sq = Bitboard::from_square(sq);
        let (left_attack, right_attack) = match color {
            Color::White => ((sq & CLEAR_FILE[0]) << 7, (sq & CLEAR_FILE[7]) << 9),
            Color::Black => ((sq & CLEAR_FILE[7]) >> 7, (sq & CLEAR_FILE[0]) >> 9),
        };
        left_attack | right_attack
    }
}

/// Generates pseudo-legal moves for sliding pieces (Rook, Bishop and Queen).
pub fn slideing_piece_pseudo_legal_moves(
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
        get_rook_moves(square, occupied) & !own
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
        get_bishop_moves(square, occupied) & !own
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
        (get_rook_moves(square, occupied) ^ get_bishop_moves(square, occupied)) & !own
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
        get_knight_moves(square) & !own
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
        get_king_moves(square) & !own
    }
    fn legal_moves(
        board: &Board,
        _: Bitboard,
        _: Bitboard,
        pieces: Bitboard,
        own_pieces: Bitboard,
        move_list: &mut MoveList,
        _: Bitboard,
    ) {
        let ksq = pieces.lsb_square();
        let bb = Self::pseudo_legal_moves(ksq, board.side_to_move, board.all_pieces(), own_pieces);

        for sq in bb {
            if board.attacks_to(sq, board.side_to_move.opposite(), board.all_pieces()) == 0 {
                move_list.push(Move::new(ksq, sq));
            }
        }
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
