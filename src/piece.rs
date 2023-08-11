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

        let valid_moves = valid_moves | valid_attacks;

        valid_moves
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

fn sliding_piece_pseudo_moves(
    sq: usize,
    occupied: Bitboard,
    own: Bitboard,
    step: usize,
) -> Bitboard {
    let mut sliding_attacks = 0;

    for i in 0..4 {
        let i = i * 2 + step;
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
        sliding_piece_pseudo_moves(sq, occupied, own, 0)
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
        sliding_piece_pseudo_moves(sq, occupied, own, 1)
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
        sliding_piece_pseudo_moves(sq, occupied, own, 0)
            | sliding_piece_pseudo_moves(sq, occupied, own, 1)
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
