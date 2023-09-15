use crate::{Bitboard, Color, Square};
mod magic;
use magic::*;

#[inline]
pub fn get_bishop_rays(sq: Square) -> Bitboard {
    unsafe { *RAYS.get_unchecked(BISHOP).get_unchecked(sq.0 as usize) }
}

#[inline]
pub fn get_rook_rays(sq: Square) -> Bitboard {
    unsafe { *RAYS.get_unchecked(ROOK).get_unchecked(sq.0 as usize) }
}

#[inline]
pub fn get_rook_moves(sq: Square, blockers: Bitboard) -> Bitboard {
    unsafe {
        let magic: Magic = *MAGIC_NUMBERS
            .get_unchecked(ROOK)
            .get_unchecked(sq.0 as usize);
        *MOVES.get_unchecked(
            (magic.offset as usize)
                + (magic.magic_number * (blockers & magic.mask)).to_size(magic.rightshift),
        ) & get_rook_rays(sq)
    }
}

#[inline]
pub fn get_bishop_moves(sq: Square, blockers: Bitboard) -> Bitboard {
    unsafe {
        let magic: Magic = *MAGIC_NUMBERS
            .get_unchecked(BISHOP)
            .get_unchecked(sq.0 as usize);
        *MOVES.get_unchecked(
            (magic.offset as usize)
                + (magic.magic_number * (blockers & magic.mask)).to_size(magic.rightshift),
        ) & get_bishop_rays(sq)
    }
}

#[inline]
pub fn get_king_moves(sq: Square) -> Bitboard {
    unsafe { *KING_MOVES.get_unchecked(sq.0 as usize) }
}

#[inline]
pub fn get_knight_moves(sq: Square) -> Bitboard {
    unsafe { *KNIGHT_MOVES.get_unchecked(sq.0 as usize) }
}

#[inline]
pub fn get_pawn_attacks(sq: Square, color: Color, blockers: Bitboard) -> Bitboard {
    unsafe {
        *PAWN_ATTACKS
            .get_unchecked(color as usize)
            .get_unchecked(sq.0 as usize)
            & blockers
    }
}

#[inline]
pub fn get_pawn_quiets(sq: Square, color: Color, blockers: Bitboard) -> Bitboard {
    unsafe {
        if (Bitboard::from_square(if color == Color::White {
            Square(sq.0 + 8)
        } else {
            Square(sq.0 - 8)
        }) & blockers)
            .0
            != 0
        {
            Bitboard(0)
        } else {
            *PAWN_MOVES
                .get_unchecked(color as usize)
                .get_unchecked(sq.0 as usize)
                & !blockers
        }
    }
}

#[inline]
pub fn get_pawn_moves(sq: Square, color: Color, blockers: Bitboard) -> Bitboard {
    get_pawn_attacks(sq, color, blockers) ^ get_pawn_quiets(sq, color, blockers)
}
