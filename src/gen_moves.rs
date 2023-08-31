use crate::{magic::*, Bitboard, Square, Color, File, Rank};

#[inline]
pub fn get_bishop_rays(sq: Square) -> Bitboard {
    unsafe { *RAYS.get_unchecked(BISHOP).get_unchecked(sq.0 as usize) }
}

/// Get the rays for a rook on a particular square.
#[inline]
pub fn get_rook_rays(sq: Square) -> Bitboard {
    unsafe { *RAYS.get_unchecked(ROOK).get_unchecked(sq.0 as usize) }
}

/// Get the moves for a rook on a particular square, given blockers blocking my movement.
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

/// Get the moves for a bishop on a particular square, given blockers blocking my movement.
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

/// Get the knight moves for a particular square.
#[inline]
pub fn get_knight_moves(sq: Square) -> Bitboard {
    unsafe { *KNIGHT_MOVES.get_unchecked(sq.0  as usize) }
}

/// Get the pawn capture move for a particular square, given the pawn's color and the potential
/// victims
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
pub fn get_castle_moves() -> Bitboard {
    CASTLE_MOVES
}

#[inline]
pub fn get_pawn_quiets(sq: Square, color: Color, blockers: Bitboard) -> Bitboard {
    unsafe {
        if (Bitboard::from_square(if color == Color::White {Square(sq.0 + 8)} else {Square(sq.0 - 8)}) & blockers).0 != 0 {
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

/// Get a line (extending to infinity, which in chess is 8 squares), given two squares.
/// This line does extend past the squares.
#[inline]
pub fn line(sq1: Square, sq2: Square) -> Bitboard {
    unsafe {
        *LINE
            .get_unchecked(sq1.0 as usize)
            .get_unchecked(sq2.0 as usize)
    }
}

/// Get a line between these two squares, not including the squares themselves.
#[inline]
pub fn between(sq1: Square, sq2: Square) -> Bitboard {
    unsafe {
        *BETWEEN
            .get_unchecked(sq1.0 as usize)
            .get_unchecked(sq2.0 as usize)
    }
}

/// Get a `BitBoard` that represents all the squares on a particular rank.
#[inline]
pub fn get_rank(rank: Rank) -> Bitboard {
    unsafe { *RANKS.get_unchecked(rank as usize) }
}

/// Get a `BitBoard` that represents all the squares on a particular file.
#[inline]
pub fn get_file(file: File) -> Bitboard {
    unsafe { *FILES.get_unchecked(file as usize) }
}

/// Get a `BitBoard` that represents the squares on the 1 or 2 files next to this file.
#[inline]
pub fn get_adjacent_files(file: File) -> Bitboard {
    unsafe { *ADJACENT_FILES.get_unchecked(file as usize) }
}

#[inline]
pub fn get_pawn_source_double_moves() -> Bitboard {
    PAWN_SOURCE_DOUBLE_MOVES
}

#[inline]
pub fn get_pawn_dest_double_moves() -> Bitboard {
    PAWN_DEST_DOUBLE_MOVES
}
