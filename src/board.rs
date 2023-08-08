use crate::{
    constants,
    moves::Move,
    parsers::fen::{self, FENParseError},
    piece::{Color, Pawn, PieceType},
    rays::RAY_ATTACKS,
    Bitboard, Square,
};
use strum::IntoEnumIterator;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Status {
    Checkmate,
    Stalemate,
    Ongoing,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct Board {
    pub white_pieces: [Bitboard; 6],
    pub black_pieces: [Bitboard; 6],
    pub move_list: Vec<Move>,
    pub side_to_move: Color,
    pub status: Status,
}

impl Board {
    pub fn new(white_pieces: [Bitboard; 6], black_pieces: [Bitboard; 6]) -> Self {
        Self {
            white_pieces,
            black_pieces,
            move_list: vec![],
            side_to_move: Color::White,
            status: Status::Ongoing,
        }
    }

    pub fn from_fen(input: &str) -> Result<Self, FENParseError> {
        let parsed = fen::parse(input)?;
        Ok(Self {
            white_pieces: parsed.pieces[Color::White as usize],
            black_pieces: parsed.pieces[Color::Black as usize],
            move_list: vec![],
            side_to_move: parsed.active_color,
            status: Status::Ongoing,
        })
    }

    #[inline]
    pub fn white(&self) -> Bitboard {
        self.white_pieces
            .into_iter()
            .reduce(|acc, next| acc | next)
            .unwrap()
    }

    #[inline]
    pub fn black(&self) -> Bitboard {
        self.black_pieces
            .into_iter()
            .reduce(|bb, next| bb | next)
            .unwrap()
    }

    #[inline]
    pub fn all_pieces(&self) -> Bitboard {
        self.white() | self.black()
    }

    pub fn pieces(&self, color: Color) -> [Bitboard; 6] {
        match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }
    }

    pub fn pieces_combined(&self, color: Color) -> Bitboard {
        match color {
            Color::White => self.white(),
            Color::Black => self.black(),
        }
    }

    #[inline]
    pub fn free(&self) -> Bitboard {
        !self.all_pieces()
    }

    #[inline]
    pub fn pseudo_legal_moves(&self) -> Vec<Move> {
        let (own_pieces, own_combined) = match self.side_to_move {
            Color::White => (self.white_pieces, self.white()),
            Color::Black => (self.black_pieces, self.black()),
        };
        let mut moves = vec![];
        if self.king_in_check() {
            for piece in PieceType::iter() {
                for sq in own_pieces[piece as usize] {
                    let mut bb = piece.pseudo_legal_moves(
                        sq,
                        self.side_to_move,
                        self.all_pieces(),
                        own_combined,
                    );

                    if piece == PieceType::King {
                        let opposite_side_attacks = self.opposite_side_attacks(
                            self.pieces(self.side_to_move.opposite()),
                            self.side_to_move.opposite(),
                        );
                        bb = (bb ^ opposite_side_attacks) & bb;
                        bb = (bb ^ self.protected_pieces(self.side_to_move.opposite())) & bb;
                    } else {
                        bb = bb & self.attacks_to_king();
                    }

                    self.fill_move_list(&mut moves, sq, bb, piece);
                }
            }
        } else {
            for piece in PieceType::iter() {
                for sq in own_pieces[piece as usize] {
                    let mut bb = piece.pseudo_legal_moves(
                        sq,
                        self.side_to_move,
                        self.all_pieces(),
                        own_combined,
                    );
                    if piece == PieceType::King {
                        let opposite_side_attacks = self.opposite_side_attacks(
                            self.pieces(self.side_to_move.opposite()),
                            self.side_to_move.opposite(),
                        );
                        bb = (bb ^ opposite_side_attacks) & bb;
                        bb = (bb ^ self.protected_pieces(self.side_to_move.opposite())) & bb;
                    }

                    self.fill_move_list(&mut moves, sq, bb, piece);
                    // for m in bb {
                    //     moves.push(Move::new(sq, m, piece, None, None));
                    // }
                }
            }
        }
        moves
    }

    #[inline]
    fn fill_move_list(
        &self,
        move_list: &mut Vec<Move>,
        sq: Square,
        moves: Bitboard,
        piece: PieceType,
    ) {
        for m in moves {
            let square_bitboard = Bitboard::from_square(m);
            let target = self
                .pieces(self.side_to_move.opposite())
                .iter()
                .enumerate()
                .find(|(_, b)| b.0 & square_bitboard.0 != 0)
                .map(|(i, _)| PieceType::from_index(i));
            move_list.push(Move::new(sq, m, piece, target, None));
        }
    }

    #[inline]
    pub fn pseudo_legal_moves_white(&self) -> Vec<Move> {
        let mut moves = vec![];
        for piece in PieceType::iter() {
            for sq in self.white_pieces[piece as usize] {
                for m in piece.pseudo_legal_moves(sq, Color::White, self.all_pieces(), self.white())
                {
                    moves.push(Move::new(sq, m, piece, None, None));
                }
            }
        }
        moves
    }

    #[inline]
    pub fn pseudo_legal_moves_black(&self) -> Vec<Move> {
        let mut moves = vec![];
        for piece in PieceType::iter() {
            for sq in self.black_pieces[piece as usize] {
                for m in piece.pseudo_legal_moves(sq, Color::Black, self.all_pieces(), self.black())
                {
                    moves.push(Move::new(sq, m, piece, None, None));
                }
            }
        }
        moves
    }

    #[inline]
    pub fn protected_pieces(&self, color: Color) -> Bitboard {
        let (own, enemy) = match color {
            Color::White => (self.white_pieces, self.black()),
            Color::Black => (self.black_pieces, self.white()),
        };
        let mut protected_bb = Bitboard(0);
        for piece in PieceType::iter() {
            for sq in own[piece as usize] {
                let bb = piece.pseudo_legal_moves(sq, color, self.all_pieces(), enemy)
                    & self.pieces_combined(color);
                protected_bb |= bb;
            }
        }
        protected_bb
    }

    pub fn make_move(&mut self, m: Move) -> Result<(), ()> {
        if self.pseudo_legal_moves().contains(&m) {
            unsafe { self.make_move_unchecked(m) }

            return Ok(());
        } else {
            return Err(());
        }
    }

    pub unsafe fn make_move_unchecked(&mut self, m: Move) {
        let from_bb = Bitboard(1_u64 << m.from.0);
        let to_bb = Bitboard(1_u64 << m.to.0);
        let from_to_bb = from_bb ^ to_bb;

        match self.side_to_move {
            Color::White => self.white_pieces[m.piece as usize] ^= from_to_bb,
            Color::Black => self.black_pieces[m.piece as usize] ^= from_to_bb,
        }

        if let Some(captured_piece) = m.capture {
            match self.side_to_move.opposite() {
                Color::White => self.white_pieces[captured_piece as usize] ^= to_bb,
                Color::Black => self.black_pieces[captured_piece as usize] ^= to_bb,
            }
        }

        self.side_to_move = self.side_to_move.opposite();
    }

    #[inline]
    pub fn attacks_to_king(&self) -> Bitboard {
        let mut attacks_to_king_bitboard = Bitboard(0);
        let (king_square, opposite_side, color) = match self.side_to_move {
            Color::White => (
                self.white_pieces[PieceType::King as usize],
                self.black_pieces,
                Color::White,
            ),
            Color::Black => (
                self.black_pieces[PieceType::King as usize],
                self.white_pieces,
                Color::Black,
            ),
        };
        // Skip the [`PieceType::King`], since you can not check with king.
        for piece in PieceType::iter().take(5) {
            for sq in opposite_side[piece as usize] {
                let bb = piece.pseudo_legal_moves(sq, color, self.all_pieces(), self.black());
                if piece != PieceType::Pawn && piece != PieceType::Knight {
                    for ray in 0..7 {
                        if RAY_ATTACKS[sq.0 as usize][ray] & bb.0 & king_square.0 != 0 {
                            attacks_to_king_bitboard |= RAY_ATTACKS[sq.0 as usize][ray] & bb.0;
                        }
                    }
                    // if king_square.0 & bb.0 > 0 {
                    //     attacks_to_king_bitboard |= bb;
                    // }
                }
                // attacks_to_king_bitboard |= king_square
                //     & piece.pseudo_legal_moves(sq, color, self.all_pieces(), self.black());
            }
        }

        attacks_to_king_bitboard
    }

    #[inline]
    pub fn king_in_check(&self) -> bool {
        let king = match self.side_to_move {
            Color::White => self.white_pieces[PieceType::King as usize],
            Color::Black => self.black_pieces[PieceType::King as usize],
        };

        (king.0 & self.attacks_to_king().0) > 1
    }

    #[inline]
    pub fn opposite_side_attacks(
        &self,
        opposite_side: [Bitboard; 6],
        opposite_color: Color,
    ) -> Bitboard {
        let mut bb = Bitboard(0);

        for sq in opposite_side[PieceType::Pawn as usize] {
            bb |= Pawn::pawn_attacks(opposite_color, sq);
        }
        for piece in PieceType::iter().skip(1) {
            for sq in opposite_side[piece as usize] {
                bb |= piece.pseudo_legal_moves(sq, opposite_color, self.all_pieces(), self.black());
            }
        }

        bb
    }
}

impl Default for Board {
    fn default() -> Self {
        Self {
            white_pieces: [
                Bitboard(constants::WHITE_PAWNS),
                Bitboard(constants::WHITE_ROOKS),
                Bitboard(constants::WHITE_KNIGHTS),
                Bitboard(constants::WHITE_BISHOPS),
                Bitboard(constants::WHITE_QUEENS),
                Bitboard(constants::WHITE_KING),
            ],
            black_pieces: [
                Bitboard(constants::BLACK_PAWNS),
                Bitboard(constants::BLACK_ROOKS),
                Bitboard(constants::BLACK_KNIGHTS),
                Bitboard(constants::BLACK_BISHOPS),
                Bitboard(constants::BLACK_QUEENS),
                Bitboard(constants::BLACK_KING),
            ],
            move_list: vec![],
            side_to_move: Color::White,
            status: Status::Ongoing,
        }
    }
}
