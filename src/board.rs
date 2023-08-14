use rand::seq::SliceRandom;
use std::str::FromStr;

use crate::{
    constants::{self, CLEAR_FILE},
    moves::{CastlingSide, Move, MoveType},
    parsers::fen::{self, FENParseError, FEN},
    piece::{Color, Pawn, PieceType},
    rays::RAY_ATTACKS,
    Bitboard, Rank, Square,
};
use strum::IntoEnumIterator;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Status {
    Checkmate(Color),
    Stalemate,
    Draw(DrawReason),
    Ongoing,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum DrawReason {
    Halfmoves,
    Repetition,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct Board {
    pub white_pieces: [Bitboard; 6],
    pub black_pieces: [Bitboard; 6],
    pub move_list: Vec<Move>,
    pub halfmoves: u16,
    pub side_to_move: Color,
    pub status: Status,
}

impl Board {
    pub fn new(white_pieces: [Bitboard; 6], black_pieces: [Bitboard; 6]) -> Self {
        Self {
            white_pieces,
            black_pieces,
            move_list: vec![],
            halfmoves: 0,
            side_to_move: Color::White,
            status: Status::Ongoing,
        }
    }

    pub fn from_fen(input: &str) -> Result<Self, FENParseError> {
        let parsed = fen::parse(input)?;
        Ok(Self::from(parsed))
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
    pub fn legal_moves(&mut self) -> Vec<Move> {
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
                        let opposite_side_attacks = self.attacks(
                            self.pieces(self.side_to_move.opposite()),
                            self.side_to_move.opposite(),
                        );
                        bb = (bb ^ opposite_side_attacks) & bb;
                        bb = (bb ^ self.protected_pieces(self.side_to_move.opposite())) & bb;
                    } else {
                        let attacks_to_king = self.attacks_to_king(self.all_pieces(), true);
                        bb = bb
                            & if attacks_to_king.1 {
                                Bitboard(0)
                            } else {
                                attacks_to_king.0
                            };

                        if let Some(pin) = self.pinned(sq) {
                            bb &= pin;
                        }
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
                    if let Some(pin) = self.pinned(sq) {
                        bb &= pin;
                    }
                    if piece == PieceType::King {
                        let opposite_side_attacks = self.attacks(
                            self.pieces(self.side_to_move.opposite()),
                            self.side_to_move.opposite(),
                        );
                        bb = (bb ^ opposite_side_attacks) & bb;
                        bb = (bb ^ self.protected_pieces(self.side_to_move.opposite())) & bb;
                    }

                    self.fill_move_list(&mut moves, sq, bb, piece);
                }
            }
            match (self.side_to_move, self.available_castling()) {
                (Color::White, Some(castling)) => match castling {
                    CastlingSide::KingSide => {
                        moves.push(Move::new(
                            Square::from_str("e1").unwrap(),
                            Square::from_str("g1").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling { side: castling },
                        ));
                    }
                    CastlingSide::QueenSide => {
                        moves.push(Move::new(
                            Square::from_str("e1").unwrap(),
                            Square::from_str("c1").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling { side: castling },
                        ));
                    }
                    CastlingSide::Both => {
                        moves.push(Move::new(
                            Square::from_str("e1").unwrap(),
                            Square::from_str("g1").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling {
                                side: CastlingSide::KingSide,
                            },
                        ));
                        moves.push(Move::new(
                            Square::from_str("e1").unwrap(),
                            Square::from_str("c1").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling {
                                side: CastlingSide::QueenSide,
                            },
                        ));
                    }
                },
                (Color::Black, Some(castling)) => match castling {
                    CastlingSide::KingSide => {
                        moves.push(Move::new(
                            Square::from_str("e8").unwrap(),
                            Square::from_str("g8").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling { side: castling },
                        ));
                    }
                    CastlingSide::QueenSide => {
                        moves.push(Move::new(
                            Square::from_str("e8").unwrap(),
                            Square::from_str("c8").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling { side: castling },
                        ));
                    }
                    CastlingSide::Both => {
                        moves.push(Move::new(
                            Square::from_str("e8").unwrap(),
                            Square::from_str("g8").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling {
                                side: CastlingSide::KingSide,
                            },
                        ));
                        moves.push(Move::new(
                            Square::from_str("e8").unwrap(),
                            Square::from_str("c8").unwrap(),
                            PieceType::King,
                            None,
                            MoveType::Castling {
                                side: CastlingSide::QueenSide,
                            },
                        ));
                    }
                },
                _ => {}
            }
        }
        moves.extend(self.available_en_passant());
        self.set_status(moves.len());

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
            if piece == PieceType::Pawn
                && ((m.rank() == Rank::Rank8 && self.side_to_move == Color::White)
                    || (m.rank() == Rank::Rank1 && self.side_to_move == Color::Black))
            {
                move_list.push(Move::new(
                    sq,
                    m,
                    piece,
                    target,
                    MoveType::Promotion {
                        promotion_to: PieceType::Rook,
                    },
                ));

                move_list.push(Move::new(
                    sq,
                    m,
                    piece,
                    target,
                    MoveType::Promotion {
                        promotion_to: PieceType::Bishop,
                    },
                ));

                move_list.push(Move::new(
                    sq,
                    m,
                    piece,
                    target,
                    MoveType::Promotion {
                        promotion_to: PieceType::Knight,
                    },
                ));

                move_list.push(Move::new(
                    sq,
                    m,
                    piece,
                    target,
                    MoveType::Promotion {
                        promotion_to: PieceType::Queen,
                    },
                ));
            } else {
                move_list.push(Move::new(sq, m, piece, target, MoveType::Quiet));
            }
        }
    }

    #[inline]
    pub fn protected_pieces(&self, color: Color) -> Bitboard {
        let king = self.pieces(color.opposite())[PieceType::King as usize];
        let (own, enemy) = match color {
            Color::White => (self.white_pieces, self.black() ^ king),
            Color::Black => (self.black_pieces, self.white() ^ king),
        };
        let mut protected_bb = Bitboard(0);
        for piece in PieceType::iter() {
            for sq in own[piece as usize] {
                let bb = piece.pseudo_legal_moves(sq, color, self.all_pieces() ^ king, enemy)
                    & self.pieces_combined(color);
                protected_bb |= bb;
            }
        }
        protected_bb
    }

    pub fn make_move(&mut self, m: Move) -> Result<(), ()> {
        if self.legal_moves().contains(&m) {
            unsafe { self.make_move_unchecked(m) }
            return Ok(());
        } else {
            return Err(());
        }
    }

    /// Updates all the bitboards, which are involved in move, updates side to move and moves list.
    ///
    /// # Safety
    /// This method does not check if provided move is a valid move, it may break representation of
    /// the game. Use it only with moves, received from [`Self::pseudo_legal_moves`]. Otherwise use [`Self::make_move`].
    pub unsafe fn make_move_unchecked(&mut self, m: Move) {
        if m.capture.is_none() && m.piece != PieceType::Pawn {
            self.halfmoves += 1;

            if self.halfmoves == 100 {
                self.status = Status::Draw(DrawReason::Halfmoves);
            }
        }
        m.update_position(
            &mut self.white_pieces,
            &mut self.black_pieces,
            self.side_to_move,
        );
        self.side_to_move = self.side_to_move.opposite();
        self.move_list.push(m);
    }

    #[inline]
    pub fn attacks_to_king(&self, all_pieces: Bitboard, with_attacker: bool) -> (Bitboard, bool) {
        let mut checks = 0;
        let mut attacks_to_king_bitboard = Bitboard(0);
        let (king_square, opposite_side, color) = match self.side_to_move {
            Color::White => (
                self.white_pieces[PieceType::King as usize],
                self.black_pieces,
                Color::Black,
            ),
            Color::Black => (
                self.black_pieces[PieceType::King as usize],
                self.white_pieces,
                Color::White,
            ),
        };
        // Skip the [`PieceType::King`], since you can not check with king.
        for piece in PieceType::iter().take(5) {
            for sq in opposite_side[piece as usize] {
                let bb =
                    piece.pseudo_legal_moves(sq, color, all_pieces, self.pieces_combined(color));
                if piece != PieceType::Pawn && piece != PieceType::Knight {
                    for ray in 0..8 {
                        if RAY_ATTACKS[sq.0 as usize][ray] & bb.0 & king_square.0 != 0 {
                            checks += 1;
                            let attacks = if with_attacker {
                                (RAY_ATTACKS[sq.0 as usize][ray] & bb.0)
                                    | Bitboard::from_square(sq).0
                            } else {
                                RAY_ATTACKS[sq.0 as usize][ray] & bb.0
                            };
                            attacks_to_king_bitboard |= attacks
                        }
                    }
                } else {
                    if bb.0 & king_square.0 != 0 {
                        checks += 1;
                        if with_attacker {
                            attacks_to_king_bitboard |= Bitboard::from_square(sq);
                        } else {
                            attacks_to_king_bitboard |= bb | Bitboard::from_square(sq);
                        }
                    }
                }
            }
        }

        (attacks_to_king_bitboard, checks > 1)
    }

    #[inline]
    pub fn king_in_check(&self) -> bool {
        let king = match self.side_to_move {
            Color::White => self.white_pieces[PieceType::King as usize],
            Color::Black => self.black_pieces[PieceType::King as usize],
        };

        (king.0 & self.attacks_to_king(self.all_pieces(), false).0 .0) != 0
    }

    /// Returns pinned ray, for given square, if piece on this square is pinned.
    /// This method checks only absolute pins, since only absolute pins are required for legal move
    /// generation.
    #[inline]
    pub fn pinned(&self, square: Square) -> Option<Bitboard> {
        // let attacks = self.attacks_to_king(self.all_pieces() ^ Bitboard::from_square(square), true);

        let attacks = self.pinned_ray(
            self.all_pieces() ^ Bitboard::from_square(square),
            true,
            square,
        );

        if attacks != 0 {
            return Some(attacks);
        }
        None
    }

    pub fn pinned_ray(
        &self,
        all_pieces: Bitboard,
        with_attacker: bool,
        square: Square,
    ) -> Bitboard {
        let mut attacks_to_king_bitboard = Bitboard(0);
        let (king_square, opposite_side, color) = match self.side_to_move {
            Color::White => (
                self.white_pieces[PieceType::King as usize],
                self.black_pieces,
                Color::Black,
            ),
            Color::Black => (
                self.black_pieces[PieceType::King as usize],
                self.white_pieces,
                Color::White,
            ),
        };
        // Skip the [`PieceType::King`], since you can not check with king.
        for piece in PieceType::iter().take(5) {
            for sq in opposite_side[piece as usize] {
                let bb =
                    piece.pseudo_legal_moves(sq, color, all_pieces, self.pieces_combined(color));
                if piece != PieceType::Pawn && piece != PieceType::Knight {
                    for ray in 0..8 {
                        if RAY_ATTACKS[sq.0 as usize][ray] & Bitboard::from_square(square).0 != 0
                            && RAY_ATTACKS[sq.0 as usize][ray] & bb.0 & king_square.0 != 0
                        {
                            let attacks = if with_attacker {
                                (RAY_ATTACKS[sq.0 as usize][ray] & bb.0)
                                    | Bitboard::from_square(sq).0
                            } else {
                                RAY_ATTACKS[sq.0 as usize][ray] & bb.0
                            };
                            attacks_to_king_bitboard |= attacks
                        }
                    }
                }
            }
        }

        attacks_to_king_bitboard
    }

    #[inline]
    pub fn attacks(&self, opposite_side: [Bitboard; 6], opposite_color: Color) -> Bitboard {
        let mut bb = Bitboard(0);

        for sq in opposite_side[PieceType::Pawn as usize] {
            bb |= Pawn::pawn_attacks(opposite_color, sq);
        }
        let king_square = self.pieces(self.side_to_move)[PieceType::King as usize];
        for piece in PieceType::iter().skip(1) {
            for sq in opposite_side[piece as usize] {
                bb |= piece.pseudo_legal_moves(sq, opposite_color, self.all_pieces(), self.black());
                bb |= piece.pseudo_legal_moves(
                    sq,
                    opposite_color,
                    self.all_pieces() ^ king_square,
                    self.black(),
                );
            }
        }

        bb
    }

    pub fn available_en_passant(&self) -> Vec<Move> {
        let mut en_passants = vec![];
        if let Some(m) = self.move_list.last() {
            let prev_move_side = self.side_to_move.opposite();
            if prev_move_side == Color::White
                && m.piece == PieceType::Pawn
                && m.from.rank() == Rank::Rank2
                && m.to.rank() == Rank::Rank4
            {
                for p in self.black_pieces[PieceType::Pawn as usize] {
                    let left_attack = Bitboard::from_square(p);

                    let (mut left_attack, mut right_attack) = (
                        (left_attack & CLEAR_FILE[0]) >> 1,
                        (left_attack & CLEAR_FILE[7]) << 1,
                    );
                    if left_attack & Bitboard::from_square(m.to) != 0 {
                        let to: Square = Bitboard(left_attack.0 >> 8).into();
                        let captures_on: Square = Bitboard(left_attack.0).into();

                        let attacks_to_king = self.attacks_to_king(self.all_pieces(), true);
                        left_attack = left_attack
                            & if attacks_to_king.1 {
                                Bitboard(0)
                            } else {
                                attacks_to_king.0
                            };

                        if let Some(pin) = self.pinned(p) {
                            left_attack &= pin;
                        }
                        if left_attack.0 != 0 {
                            en_passants.push(Move::new(
                                p,
                                to,
                                PieceType::Pawn,
                                Some(PieceType::Pawn),
                                MoveType::EnPassant { captures_on },
                            ))
                        }
                    }

                    if right_attack & Bitboard::from_square(m.to) != 0 {
                        let to: Square = Bitboard(right_attack.0 >> 8).into();
                        let captures_on: Square = Bitboard(right_attack.0).into();

                        let attacks_to_king = self.attacks_to_king(self.all_pieces(), true);
                        right_attack = right_attack
                            & if attacks_to_king.1 {
                                Bitboard(0)
                            } else {
                                attacks_to_king.0
                            };

                        if let Some(pin) = self.pinned(p) {
                            right_attack &= pin;
                        }
                        if right_attack.0 != 0 {
                            en_passants.push(Move::new(
                                p,
                                to,
                                PieceType::Pawn,
                                Some(PieceType::Pawn),
                                MoveType::EnPassant { captures_on },
                            ))
                        }
                    }
                }
            }
            if prev_move_side == Color::Black
                && m.piece == PieceType::Pawn
                && m.from.rank() == Rank::Rank7
                && m.to.rank() == Rank::Rank5
            {
                for p in self.white_pieces[PieceType::Pawn as usize] {
                    let bb = Bitboard::from_square(p);

                    let (mut right_attack, mut left_attack) =
                        ((bb & CLEAR_FILE[7]) << 1, (bb & CLEAR_FILE[0]) >> 1);

                    if left_attack & Bitboard::from_square(m.to) != 0 {
                        let to: Square = Bitboard(left_attack.0 << 8).into();
                        let captures_on: Square = Bitboard(left_attack.0).into();
                        let attacks_to_king = self.attacks_to_king(self.all_pieces(), true);
                        left_attack = left_attack
                            & if attacks_to_king.1 {
                                Bitboard(0)
                            } else {
                                attacks_to_king.0
                            };

                        if let Some(pin) = self.pinned(p) {
                            left_attack &= pin;
                        }
                        if left_attack.0 != 0 {
                            en_passants.push(Move::new(
                                p,
                                to,
                                PieceType::Pawn,
                                Some(PieceType::Pawn),
                                MoveType::EnPassant { captures_on },
                            ))
                        }
                    }

                    if right_attack & Bitboard::from_square(m.to) != 0 {
                        let to: Square = Bitboard(right_attack.0 << 8).into();
                        let captures_on: Square = Bitboard(right_attack.0).into();

                        let attacks_to_king = self.attacks_to_king(self.all_pieces(), true);
                        right_attack = right_attack
                            & if attacks_to_king.1 {
                                Bitboard(0)
                            } else {
                                attacks_to_king.0
                            };

                        if let Some(pin) = self.pinned(p) {
                            right_attack &= pin;
                        }
                        if right_attack.0 != 0 {
                            en_passants.push(Move::new(
                                p,
                                to,
                                PieceType::Pawn,
                                Some(PieceType::Pawn),
                                MoveType::EnPassant { captures_on },
                            ))
                        }
                    }
                }
            }
        }
        en_passants
    }

    fn set_status(&mut self, num_of_legal_moves: usize) {
        match (self.king_in_check(), num_of_legal_moves) {
            (true, 0) => self.status = Status::Checkmate(self.side_to_move.opposite()),
            (false, 0) => self.status = Status::Stalemate,
            _ => {}
        }
    }
    pub fn available_castling(&self) -> Option<CastlingSide> {
        if self.king_in_check() {
            return None;
        }

        let original_square = match self.side_to_move {
            Color::White => Square::from_str("e1").unwrap(),
            Color::Black => Square::from_str("e8").unwrap(),
        };
        let king_on_original_square = self
            .move_list
            .iter()
            .filter(|m| m.piece == PieceType::King && m.from == original_square)
            .count()
            == 0
            && (self.pieces(self.side_to_move)[PieceType::King as usize]
                & Bitboard::from_square(original_square)
                != 0);
        let a_rook_square = match self.side_to_move {
            Color::White => Square::from_str("a1").unwrap(),
            Color::Black => Square::from_str("a8").unwrap(),
        };
        let h_rook_square = match self.side_to_move {
            Color::White => Square::from_str("h1").unwrap(),
            Color::Black => Square::from_str("h8").unwrap(),
        };
        let a_rook_on_original_square = self
            .move_list
            .iter()
            .filter(|m| m.piece == PieceType::King && m.from == a_rook_square)
            .count()
            == 0
            && (self.pieces(self.side_to_move)[PieceType::Rook as usize]
                & Bitboard::from_square(a_rook_square)
                != 0);
        let h_rook_on_original_square = self
            .move_list
            .iter()
            .filter(|m| m.piece == PieceType::King && m.from == h_rook_square)
            .count()
            == 0
            && (self.pieces(self.side_to_move)[PieceType::Rook as usize]
                & Bitboard::from_square(h_rook_square)
                != 0);

        let pieces = self.pieces_combined(self.side_to_move)
            | self.attacks(
                self.pieces(self.side_to_move.opposite()),
                self.side_to_move.opposite(),
            );
        let king_side = match self.side_to_move {
            Color::White => (pieces.0 >> 5).trailing_zeros() == 2,
            Color::Black => (pieces.0.swap_bytes() >> 5).trailing_zeros() == 2,
        } && h_rook_on_original_square;
        let queen_side = match self.side_to_move {
            Color::White => (pieces.0 >> 1).trailing_zeros() == 3,
            Color::Black => (pieces.0.swap_bytes() >> 1).trailing_zeros() == 3,
        } && a_rook_on_original_square;

        match (king_on_original_square, king_side, queen_side) {
            (true, true, true) => Some(CastlingSide::Both),
            (true, true, false) => Some(CastlingSide::KingSide),
            (true, false, true) => Some(CastlingSide::QueenSide),
            (true, false, false) => None,
            (false, _, _) => None,
        }
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
            halfmoves: 0,
            side_to_move: Color::White,
            status: Status::Ongoing,
        }
    }
}

impl From<FEN> for Board {
    fn from(fen: FEN) -> Self {
        let mut move_list = vec![];
        if let Some(square) = fen.en_passant_target {
            match square.rank() {
                // White pawn moved 2 squares up.
                Rank::Rank3 => {
                    let from_square = Square::from_file_and_rank(square.file(), Rank::Rank2);
                    let to_square = Square::from_file_and_rank(square.file(), Rank::Rank4);
                    move_list.push(Move {
                        from: from_square,
                        to: to_square,
                        piece: PieceType::Pawn,
                        capture: None,
                        move_type: MoveType::Quiet,
                    });
                }
                // Black pawn moved 2 squares up.
                Rank::Rank6 => {
                    let from_square = Square::from_file_and_rank(square.file(), Rank::Rank7);
                    let to_square = Square::from_file_and_rank(square.file(), Rank::Rank5);
                    move_list.push(Move {
                        from: from_square,
                        to: to_square,
                        piece: PieceType::Pawn,
                        capture: None,
                        move_type: MoveType::Quiet,
                    });
                }
                _ => unreachable!(),
            }
        }
        Self {
            white_pieces: fen.pieces[Color::White as usize],
            black_pieces: fen.pieces[Color::Black as usize],
            move_list,
            halfmoves: fen.halfmove_clock,
            side_to_move: fen.active_color,
            status: if fen.halfmove_clock < 100 {
                Status::Ongoing
            } else {
                Status::Draw(DrawReason::Halfmoves)
            },
        }
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pieces = self.all_pieces().0;
        write!(f, "\n ---------------------------------\n")?;
        for row in (0..8).rev() {
            write!(f, "{}", row + 1)?;
            for i in 0..8 {
                let piece = Bitboard::from_square_number(8 * row + i);
                if piece & pieces != 0 {
                    for p in PieceType::iter() {
                        if self.white_pieces[p as usize] & piece != 0 {
                            write!(f, "| {} ", p.unicode(Color::White))?;
                            break;
                        }
                        if self.black_pieces[p as usize] & piece != 0 {
                            write!(f, "| {} ", p.unicode(Color::Black))?;
                            break;
                        }
                    }
                } else {
                    write!(f, "|   ")?;
                }
            }
            write!(f, "|")?;

            write!(f, "\n ---------------------------------\n")?;
        }
        write!(f, "   A   B   C   D   E   F   G   H ")?;
        write!(f, "\n")?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct Position {
    pub white_pieces: [Bitboard; 6],
    pub black_pieces: [Bitboard; 6],
}

impl Position {
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
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pieces = self.all_pieces().0;
        write!(f, "\n ---------------------------------\n")?;
        for row in (0..8).rev() {
            write!(f, "{}", row + 1)?;
            for i in 0..8 {
                let piece = Bitboard::from_square_number(8 * row + i);
                if piece & pieces != 0 {
                    for p in PieceType::iter() {
                        if self.white_pieces[p as usize] & piece != 0 {
                            write!(f, "| {} ", p.unicode(Color::White))?;
                            break;
                        }
                        if self.black_pieces[p as usize] & piece != 0 {
                            write!(f, "| {} ", p.unicode(Color::Black))?;
                            break;
                        }
                    }
                } else {
                    write!(f, "|   ")?;
                }
            }
            write!(f, "|")?;

            write!(f, "\n ---------------------------------\n")?;
        }
        write!(f, "   A   B   C   D   E   F   G   H ")?;
        write!(f, "\n")?;
        Ok(())
    }
}

pub struct IntoIter {
    board: Board,
}

impl IntoIterator for Board {
    type Item = Position;
    type IntoIter = IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter { board: self }
    }
}
impl Iterator for IntoIter {
    type Item = Position;
    fn next(&mut self) -> Option<Self::Item> {
        let moves = self.board.legal_moves();
        if moves.len() == 0 || self.board.move_list.len() == 250 {
            println!("{:?}", self.board.status);
            return None;
        }

        let mut rng = rand::thread_rng();
        let m = if let Some(m) = moves.iter().find(|m| m.capture == Some(PieceType::King)) {
            m.to_owned()
        } else {
            let captures: Vec<&Move> = moves.iter().filter(|m| m.capture.is_some()).collect();
            if captures.len() != 0 {
                captures.choose(&mut rng).unwrap().to_owned().to_owned()
            } else {
                moves.choose(&mut rng).unwrap().to_owned()
            }
        };
        let _ = unsafe { self.board.make_move_unchecked(m) };
        Some(Position {
            white_pieces: self.board.white_pieces,
            black_pieces: self.board.black_pieces,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn halfmoves_rule() {
        let board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 100 1").unwrap();
        assert_eq!(board.status, Status::Draw(DrawReason::Halfmoves));
    }

    #[test]
    fn combined_pieces() {
        let board = Board::default();
        assert_eq!(board.white().0, 0xFFFF);
        assert_eq!(board.black().0, 0xFFFF_u64.swap_bytes());
        assert_eq!(board.all_pieces().0, 0xFFFF | 0xFFFF_u64.swap_bytes());
    }

    #[test]
    fn legal_moves_in_starting_position() {
        let mut board = Board::default();
        let moves = board.legal_moves();
        assert_eq!(moves.len(), 20);
        assert_eq!(board.side_to_move, Color::White);
        let _ = board.make_move(moves[0]);
        let moves = board.legal_moves();
        assert_eq!(moves.len(), 20);
        assert_eq!(board.side_to_move, Color::Black);
    }
}
