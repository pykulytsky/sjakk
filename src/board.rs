use rand::seq::SliceRandom;
use smallvec::{smallvec, SmallVec};
use thiserror::Error;

use crate::{
    constants::{self, CLEAR_FILE},
    moves::{CastlingSide, Move, MoveType},
    parsers::fen::{self, FENParseError, FEN},
    piece::{Color, Pawn, PieceType},
    rays::{BISHOP_ATTACKS, RAY_ATTACKS, ROOK_ATTACKS},
    utils::between,
    Bitboard, Rank, Square,
};

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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Default)]
pub struct CastlingRights {
    white_king_moved: bool,
    black_king_moved: bool,

    white_a_rook_moved: bool,
    white_h_rook_moved: bool,

    black_a_rook_moved: bool,
    black_h_rook_moved: bool,
}

impl From<(Option<CastlingSide>, Option<CastlingSide>)> for CastlingRights {
    fn from((white, black): (Option<CastlingSide>, Option<CastlingSide>)) -> Self {
        let mut rules = CastlingRights::default();
        match (white, black) {
            (None, None) => {
                rules.white_king_moved = true;
                rules.black_king_moved = true;
            }
            (None, Some(side)) => {
                rules.white_king_moved = true;
                match side {
                    CastlingSide::KingSide => {
                        rules.black_a_rook_moved = true;
                    }
                    CastlingSide::QueenSide => {
                        rules.black_h_rook_moved = true;
                    }
                    CastlingSide::Both => {}
                }
            }
            (Some(side), None) => {
                rules.black_king_moved = true;
                match side {
                    CastlingSide::KingSide => {
                        rules.white_a_rook_moved = true;
                    }
                    CastlingSide::QueenSide => {
                        rules.white_h_rook_moved = true;
                    }
                    CastlingSide::Both => {}
                }
            }
            (Some(side), Some(side_black)) => {
                match side {
                    CastlingSide::KingSide => {
                        rules.white_a_rook_moved = true;
                    }
                    CastlingSide::QueenSide => {
                        rules.white_h_rook_moved = true;
                    }
                    CastlingSide::Both => {}
                }

                match side_black {
                    CastlingSide::KingSide => {
                        rules.black_a_rook_moved = true;
                    }
                    CastlingSide::QueenSide => {
                        rules.black_h_rook_moved = true;
                    }
                    CastlingSide::Both => {}
                }
            }
        }

        rules
    }
}

impl CastlingRights {
    fn king_moved(&self, side: Color) -> bool {
        match side {
            Color::White => self.white_king_moved,
            Color::Black => self.black_king_moved,
        }
    }

    fn a_rook_moved(&self, side: Color) -> bool {
        match side {
            Color::White => self.white_a_rook_moved,
            Color::Black => self.black_a_rook_moved,
        }
    }
    fn h_rook_moved(&self, side: Color) -> bool {
        match side {
            Color::White => self.white_h_rook_moved,
            Color::Black => self.black_h_rook_moved,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct Board {
    pub white_pieces: [Bitboard; 6],
    pub black_pieces: [Bitboard; 6],
    pub white: Bitboard,
    pub black: Bitboard,
    pub move_list: SmallVec<[Move; 256]>,
    pub halfmoves: u16,
    pub side_to_move: Color,
    pub status: Status,
    pub castling_rights: CastlingRights,
}

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Illegal move")]
pub struct IllegalMove;

impl Board {
    pub fn new(white_pieces: [Bitboard; 6], black_pieces: [Bitboard; 6]) -> Self {
        Self {
            white_pieces,
            black_pieces,
            white: Self::combine(white_pieces),
            black: Self::combine(black_pieces),
            move_list: smallvec![],
            halfmoves: 0,
            side_to_move: Color::White,
            status: Status::Ongoing,
            castling_rights: CastlingRights::default(),
        }
    }

    pub fn from_fen(input: &str) -> Result<Self, FENParseError> {
        let parsed = fen::parse(input)?;
        Ok(Self::from(parsed))
    }

    #[inline]
    pub fn combine(bb: [Bitboard; 6]) -> Bitboard {
        bb.into_iter().reduce(|acc, next| acc | next).unwrap()
    }

    #[inline]
    pub fn all_pieces(&self) -> Bitboard {
        self.white | self.black
    }

    fn pieces(&self, color: Color) -> [Bitboard; 6] {
        match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }
    }

    pub fn pieces_combined(&self, color: Color) -> Bitboard {
        match color {
            Color::White => self.white,
            Color::Black => self.black,
        }
    }

    #[inline]
    pub fn free(&self) -> Bitboard {
        !self.all_pieces()
    }

    #[inline]
    pub fn legal_moves(&mut self) -> SmallVec<[Move; 32]> {
        let pinned_bb = self.find_pinned();
        let (own_pieces, own_combined) = match self.side_to_move {
            Color::White => (self.white_pieces, self.white),
            Color::Black => (self.black_pieces, self.black),
        };
        let mut moves = smallvec![];

        let (king_in_check, attacks_to_king) = self.king_in_check();
        let attacks = self.attacks(
            self.pieces(self.side_to_move.opposite()),
            self.side_to_move.opposite(),
        );
        let opposite_side_attacks = attacks.0;
        let protected_pieces = attacks.1;

        for piece in PieceType::ALL {
            for sq in own_pieces[piece as usize] {
                let mut bb = piece.pseudo_legal_moves(
                    sq,
                    self.side_to_move,
                    self.all_pieces(),
                    own_combined,
                );
                if bb == 0 {
                    continue;
                }

                if king_in_check {
                    if piece == PieceType::King {
                        bb = (bb ^ opposite_side_attacks) & bb;
                        bb = (bb ^ protected_pieces) & bb;
                    } else {
                        bb &= if attacks_to_king.1 {
                            Bitboard(0)
                        } else {
                            attacks_to_king.0
                        };
                    }
                } else if piece == PieceType::King {
                    bb = (bb ^ opposite_side_attacks) & bb;
                    bb = (bb ^ protected_pieces) & bb;
                }

                let pinned = pinned_bb & Bitboard::from_square(sq) != 0;
                if pinned {
                    let king_square =
                        self.pieces(self.side_to_move)[PieceType::King as usize].lsb_square();
                    let pin = self.get_ray(king_square, sq);
                    bb &= pin;
                }
                if bb != 0 {
                    self.fill_move_list(&mut moves, sq, bb, piece);
                }
            }
        }

        if !king_in_check {
            self.castling_rules(&mut moves, &opposite_side_attacks);
        }
        moves.extend(self.available_en_passant());
        self.set_status(moves.len(), king_in_check);

        moves
    }

    fn castling_rules(&self, moves: &mut SmallVec<[Move; 32]>, opposite_side_attacks: &Bitboard) {
        match (
            self.side_to_move,
            self.available_castling(opposite_side_attacks),
        ) {
            (Color::White, Some(castling)) => match castling {
                CastlingSide::KingSide => {
                    moves.push(Move::new(
                        Square::E1,
                        Square::G1,
                        PieceType::King,
                        None,
                        MoveType::Castling { side: castling },
                    ));
                }
                CastlingSide::QueenSide => {
                    moves.push(Move::new(
                        Square::E1,
                        Square::C1,
                        PieceType::King,
                        None,
                        MoveType::Castling { side: castling },
                    ));
                }
                CastlingSide::Both => {
                    moves.push(Move::new(
                        Square::E1,
                        Square::G1,
                        PieceType::King,
                        None,
                        MoveType::Castling {
                            side: CastlingSide::KingSide,
                        },
                    ));
                    moves.push(Move::new(
                        Square::E1,
                        Square::C1,
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
                        Square::E8,
                        Square::G8,
                        PieceType::King,
                        None,
                        MoveType::Castling { side: castling },
                    ));
                }
                CastlingSide::QueenSide => {
                    moves.push(Move::new(
                        Square::E8,
                        Square::C8,
                        PieceType::King,
                        None,
                        MoveType::Castling { side: castling },
                    ));
                }
                CastlingSide::Both => {
                    moves.push(Move::new(
                        Square::E8,
                        Square::G8,
                        PieceType::King,
                        None,
                        MoveType::Castling {
                            side: CastlingSide::KingSide,
                        },
                    ));
                    moves.push(Move::new(
                        Square::E8,
                        Square::C8,
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

    #[inline]
    fn fill_move_list(
        &self,
        move_list: &mut SmallVec<[Move; 32]>,
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
    pub fn attacks(&self, side: [Bitboard; 6], color: Color) -> (Bitboard, Bitboard) {
        let king = self.pieces(color.opposite())[PieceType::King as usize];
        let (own, enemy) = match color {
            Color::White => (self.white_pieces, self.black ^ king),
            Color::Black => (self.black_pieces, self.white ^ king),
        };
        let mut attacks_bb = Bitboard(0);
        let mut protected_bb = Bitboard(0);
        for sq in side[PieceType::Pawn as usize] {
            attacks_bb |= Pawn::pawn_attacks(color, sq);
        }

        let king_square = self.pieces(self.side_to_move)[PieceType::King as usize];
        for piece in PieceType::ALL {
            for sq in own[piece as usize] {
                if piece != PieceType::Pawn {
                    let bb = piece.pseudo_legal_moves(
                        sq,
                        color,
                        self.all_pieces() ^ king_square,
                        self.pieces_combined(color),
                    );
                    attacks_bb |= bb;
                }
                let bb = piece.pseudo_legal_moves(sq, color, self.all_pieces() ^ king, enemy)
                    & self.pieces_combined(color);
                protected_bb |= bb;
            }
        }
        (attacks_bb, protected_bb)
    }

    #[inline]
    pub fn protected_pieces(&self, color: Color) -> Bitboard {
        let king = self.pieces(color.opposite())[PieceType::King as usize];
        let (own, enemy) = match color {
            Color::White => (self.white_pieces, self.black ^ king),
            Color::Black => (self.black_pieces, self.white ^ king),
        };
        let mut protected_bb = Bitboard(0);
        for piece in PieceType::ALL {
            for sq in own[piece as usize] {
                let bb = piece.pseudo_legal_moves(sq, color, self.all_pieces() ^ king, enemy)
                    & self.pieces_combined(color);
                protected_bb |= bb;
            }
        }
        protected_bb
    }

    pub fn make_move(&mut self, m: Move) -> Result<(), IllegalMove> {
        if self.legal_moves().contains(&m) {
            unsafe { self.make_move_unchecked(m) }
            return Ok(());
        }

        Err(IllegalMove)
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
        // upadte castling rights
        match m.piece {
            PieceType::Rook => match m.from {
                Square::A1 => {
                    self.castling_rights.white_a_rook_moved = true;
                }
                Square::A8 => {
                    self.castling_rights.black_a_rook_moved = true;
                }
                Square::H1 => {
                    self.castling_rights.white_h_rook_moved = true;
                }
                Square::H8 => {
                    self.castling_rights.black_h_rook_moved = true;
                }
                _ => {}
            },
            PieceType::King => match m.from {
                Square::E1 => {
                    self.castling_rights.white_king_moved = true;
                }
                Square::E8 => {
                    self.castling_rights.black_king_moved = true;
                }
                _ => {}
            },
            _ => {}
        }

        if let Some(PieceType::Rook) = m.capture {
            match m.to {
                Square::A1 => {
                    self.castling_rights.white_a_rook_moved = true;
                }
                Square::A8 => {
                    self.castling_rights.black_a_rook_moved = true;
                }
                Square::H1 => {
                    self.castling_rights.white_h_rook_moved = true;
                }
                Square::H8 => {
                    self.castling_rights.black_h_rook_moved = true;
                }
                _ => {}
            }
        }
        m.update_position(
            &mut self.white_pieces,
            &mut self.black_pieces,
            &mut self.white,
            &mut self.black,
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
        for piece in PieceType::ALL.into_iter().take(5) {
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
                } else if bb.0 & king_square.0 != 0 {
                    checks += 1;
                    if with_attacker {
                        attacks_to_king_bitboard |= (bb & king_square) | Bitboard::from_square(sq);
                    } else {
                        attacks_to_king_bitboard |= bb & king_square;
                    }
                }
            }
        }

        (attacks_to_king_bitboard, checks > 1)
    }

    /// Returns true if king in check, second tuple contains [`Bitboard`] with all the attacks to
    /// king with attacking pieces, as well as boolean value which indicates if king is in double
    /// check.
    #[inline]
    pub fn king_in_check(&self) -> (bool, (Bitboard, bool)) {
        let king = match self.side_to_move {
            Color::White => self.white_pieces[PieceType::King as usize],
            Color::Black => self.black_pieces[PieceType::King as usize],
        };

        let attacks = self.attacks_to_king(self.all_pieces(), true);
        let attacks_to_king =
            attacks.0 & !(attacks.0 & self.pieces_combined(self.side_to_move.opposite()));

        ((king & attacks_to_king) != 0, attacks)
    }

    /// Returns pinned ray, for given square, if piece on this square is pinned.
    /// This method checks only absolute pins, since only absolute pins are required for legal move
    /// generation.
    #[inline]
    fn pinned(&self, square: Square) -> Option<Bitboard> {
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
        let rays_to_king = RAY_ATTACKS[king_square.lsb_square().0 as usize]
            .into_iter()
            .reduce(|acc, next| acc | next)
            .unwrap();
        for piece in [1, 3, 4] {
            let piece = PieceType::from_index(piece);
            for sq in opposite_side[piece as usize] & rays_to_king {
                let bb =
                    piece.pseudo_legal_moves(sq, color, all_pieces, self.pieces_combined(color));
                for ray in 0..8 {
                    if RAY_ATTACKS[sq.0 as usize][ray] & Bitboard::from_square(square).0 != 0
                        && RAY_ATTACKS[sq.0 as usize][ray] & bb.0 & king_square.0 != 0
                    {
                        let attacks = if with_attacker {
                            (RAY_ATTACKS[sq.0 as usize][ray] & bb.0) | Bitboard::from_square(sq).0
                        } else {
                            RAY_ATTACKS[sq.0 as usize][ray] & bb.0
                        };
                        attacks_to_king_bitboard |= attacks
                    }
                }
            }
        }

        attacks_to_king_bitboard
    }

    pub fn available_en_passant(&self) -> SmallVec<[Move; 2]> {
        let mut en_passants = smallvec![];
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
                        left_attack &= if attacks_to_king.1 {
                            Bitboard(0)
                        } else if attacks_to_king.0 == 0 {
                            Bitboard::universe()
                        } else if Bitboard::from_square(to) & attacks_to_king.0 == 0 {
                            Bitboard(0)
                        } else {
                            Bitboard::universe()
                        };

                        if let Some(pin) = self.pinned(p) {
                            if Bitboard::from_square(to) & pin == 0 {
                                left_attack &= pin;
                            }
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
                        right_attack &= if attacks_to_king.1 {
                            Bitboard(0)
                        } else if attacks_to_king.0 == 0 {
                            Bitboard::universe()
                        } else if Bitboard::from_square(to) & attacks_to_king.0 == 0 {
                            Bitboard(0)
                        } else {
                            Bitboard::universe()
                        };

                        if let Some(pin) = self.pinned(p) {
                            if Bitboard::from_square(to) & pin == 0 {
                                right_attack &= pin;
                            }
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

                        left_attack &= if attacks_to_king.1 {
                            Bitboard(0)
                        } else if attacks_to_king.0 == 0 {
                            Bitboard::universe()
                        } else if Bitboard::from_square(to) & attacks_to_king.0 == 0 {
                            Bitboard(0)
                        } else {
                            Bitboard::universe()
                        };

                        if let Some(pin) = self.pinned(p) {
                            if Bitboard::from_square(to) & pin == 0 {
                                left_attack &= pin;
                            }
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
                        right_attack &= if attacks_to_king.1 {
                            Bitboard(0)
                        } else if attacks_to_king.0 == 0 {
                            Bitboard::universe()
                        } else if Bitboard::from_square(to) & attacks_to_king.0 == 0 {
                            Bitboard(0)
                        } else {
                            Bitboard::universe()
                        };
                        if let Some(pin) = self.pinned(p) {
                            if Bitboard::from_square(to) & pin == 0 {
                                right_attack &= pin;
                            }
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

    fn set_status(&mut self, num_of_legal_moves: usize, king_in_check: bool) {
        match (king_in_check, num_of_legal_moves) {
            (true, 0) => self.status = Status::Checkmate(self.side_to_move.opposite()),
            (false, 0) => self.status = Status::Stalemate,
            _ => {}
        }
    }

    pub fn find_pinned(&self) -> Bitboard {
        let king = self.pieces(self.side_to_move)[PieceType::King as usize].lsb_square();
        //white -> opposite
        let bishop_rays = Bitboard(
            BISHOP_ATTACKS[king.0 as usize]
                .into_iter()
                .reduce(|acc, next| acc | next)
                .unwrap()
                .to_owned(),
        ) & (self.pieces(self.side_to_move.opposite())
            [PieceType::Bishop as usize]
            | self.pieces(self.side_to_move.opposite())[PieceType::Queen as usize]);

        let rook_rays = Bitboard(
            ROOK_ATTACKS[king.0 as usize]
                .into_iter()
                .reduce(|acc, next| acc | next)
                .unwrap()
                .to_owned(),
        ) & (self.pieces(self.side_to_move.opposite())[PieceType::Rook as usize]
            | self.pieces(self.side_to_move.opposite())[PieceType::Queen as usize]);
        let attackers =
            self.pieces_combined(self.side_to_move.opposite()) & (bishop_rays | rook_rays);
        let mut pinned = Bitboard(0);
        for sq in attackers {
            let between = between(sq, king) & self.all_pieces();
            if between.0.count_ones() == 1 {
                pinned ^= between;
            }
        }
        pinned
    }

    fn get_ray(&self, sq1: Square, sq2: Square) -> Bitboard {
        Bitboard(
            RAY_ATTACKS[sq1.0 as usize]
                .into_iter()
                .find(|r| r & Bitboard::from_square(sq2).0 != 0)
                .unwrap(),
        )
    }

    pub fn get_available_castling(&self) -> Option<CastlingSide> {
        let opposite_side_attacks = self.attacks(
            self.pieces(self.side_to_move.opposite()),
            self.side_to_move.opposite(),
        );
        self.available_castling(&opposite_side_attacks.0)
    }

    #[inline]
    fn available_castling(&self, opposite_side_attacks: &Bitboard) -> Option<CastlingSide> {
        let king_on_original_square = !self.castling_rights.king_moved(self.side_to_move);
        let a_rook_on_original_square = !self.castling_rights.a_rook_moved(self.side_to_move);
        let h_rook_on_original_square = !self.castling_rights.h_rook_moved(self.side_to_move);

        let pieces = &self.pieces_combined(self.side_to_move) | opposite_side_attacks;
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
        let white_pieces = [
            Bitboard(constants::WHITE_PAWNS),
            Bitboard(constants::WHITE_ROOKS),
            Bitboard(constants::WHITE_KNIGHTS),
            Bitboard(constants::WHITE_BISHOPS),
            Bitboard(constants::WHITE_QUEENS),
            Bitboard(constants::WHITE_KING),
        ];
        let black_pieces = [
            Bitboard(constants::BLACK_PAWNS),
            Bitboard(constants::BLACK_ROOKS),
            Bitboard(constants::BLACK_KNIGHTS),
            Bitboard(constants::BLACK_BISHOPS),
            Bitboard(constants::BLACK_QUEENS),
            Bitboard(constants::BLACK_KING),
        ];
        let white = Self::combine(white_pieces);
        let black = Self::combine(black_pieces);
        Self {
            white_pieces,
            black_pieces,
            white,
            black,
            move_list: smallvec![],
            halfmoves: 0,
            side_to_move: Color::White,
            status: Status::Ongoing,
            castling_rights: CastlingRights::default(),
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
        let white = Self::combine(fen.pieces[Color::White as usize]);
        let black = Self::combine(fen.pieces[Color::Black as usize]);
        Self {
            white_pieces: fen.pieces[Color::White as usize],
            black_pieces: fen.pieces[Color::Black as usize],
            white,
            black,
            move_list: move_list.into(),
            halfmoves: fen.halfmove_clock,
            side_to_move: fen.active_color,
            status: if fen.halfmove_clock < 100 {
                Status::Ongoing
            } else {
                Status::Draw(DrawReason::Halfmoves)
            },
            // TODO: respect here castling rights from FEN
            castling_rights: CastlingRights::from(fen.castling_rules),
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
                    for p in PieceType::ALL {
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
        writeln!(f)?;
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
                    for p in PieceType::ALL {
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
        writeln!(f)?;
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
        if moves.is_empty() || self.board.move_list.len() == 250 {
            println!("{:?}", self.board.status);
            return None;
        }

        let mut rng = rand::thread_rng();
        let m = if let Some(m) = moves.iter().find(|m| {
            m.move_type
                == MoveType::Castling {
                    side: CastlingSide::KingSide,
                }
        }) {
            m.to_owned()
        } else {
            let captures: Vec<&Move> = moves.iter().filter(|m| m.capture.is_some()).collect();
            if !captures.is_empty() {
                captures.choose(&mut rng).unwrap().to_owned().to_owned()
            } else {
                moves.choose(&mut rng).unwrap().to_owned()
            }
        };
        unsafe {
            self.board.make_move_unchecked(m);
        }
        Some(Position {
            white_pieces: self.board.white_pieces,
            black_pieces: self.board.black_pieces,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn halfmoves_rule() {
        let board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 100 1").unwrap();
        assert_eq!(board.status, Status::Draw(DrawReason::Halfmoves));
    }

    #[test]
    fn en_passant_target() {
        let board =
            Board::from_fen("rnbqkbnr/p1p1pppp/1p6/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3")
                .unwrap();
        assert_eq!(board.move_list.len(), 1);
        assert_eq!(board.move_list[0].piece, PieceType::Pawn);
        assert_eq!(board.move_list[0].from, Square::from_str("d7").unwrap());
        assert_eq!(board.move_list[0].to, Square::from_str("d5").unwrap());
        let moves = board.available_en_passant();
        assert!(moves.iter().any(|m| m.move_type
            == MoveType::EnPassant {
                captures_on: Square::from_str("d5").unwrap()
            }))
    }

    #[test]
    fn en_passant_under_pin() {
        let board = Board::from_fen("3kq3/8/8/4Pp2/8/8/8/4K3 w - f6 0 1").unwrap();
        let moves = board.available_en_passant();

        assert!(board.available_en_passant().is_empty());
        assert!(board.pinned(Square::from_str("e5").unwrap()).is_some());
        assert!(!moves.iter().any(|m| m.move_type
            == MoveType::EnPassant {
                captures_on: Square::from_str("f5").unwrap()
            }))
    }

    #[test]
    fn en_passant_under_pin_in_ray() {
        let board = Board::from_fen("2k4b/8/8/4Pp2/8/8/1K6/8 w - f6 0 1").unwrap();
        let moves = board.available_en_passant();

        assert!(!board.available_en_passant().is_empty());
        assert!(board.pinned(Square::from_str("e5").unwrap()).is_some());
        assert!(moves.iter().any(|m| m.move_type
            == MoveType::EnPassant {
                captures_on: Square::from_str("f5").unwrap()
            }))
    }

    #[test]
    fn en_passant_under_pin_target_square_in_ray() {
        let board = Board::from_fen("8/8/1K5q/3pP3/8/8/8/7k w - d6 0 1").unwrap();
        let moves = board.available_en_passant();

        assert!(!board.available_en_passant().is_empty());
        assert!(moves.iter().any(|m| m.move_type
            == MoveType::EnPassant {
                captures_on: Square::from_str("d5").unwrap()
            }))
    }

    #[test]
    fn combined_pieces() {
        let board = Board::default();
        assert_eq!(board.white.0, 0xFFFF);
        assert_eq!(board.black.0, 0xFFFF_u64.swap_bytes());
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

    #[test]
    fn pin() {
        let mut board = Board::from_fen("4k3/8/7b/8/8/4P3/3K4/8 w - - 0 1").unwrap();
        let moves = board.legal_moves();

        assert!(board.pinned(Square::from_str("e3").unwrap()).is_some());
        assert_eq!(
            moves.iter().filter(|m| m.piece == PieceType::Pawn).count(),
            0
        );
    }

    #[test]
    fn ray_blocked_by_two_pieces() {
        let board = Board::from_fen("4k3/8/7b/8/5P2/4P3/3K4/8 w - - 0 1").unwrap();

        assert!(board.pinned(Square::from_str("e3").unwrap()).is_none());
        assert!(board.pinned(Square::from_str("e4").unwrap()).is_none());
    }

    #[test]
    fn pinned_piece_can_not_capture_attacking_piece() {
        let mut board = Board::from_fen("8/3k4/4p3/3R4/8/7B/8/6K1 b - - 0 1").unwrap();
        let moves = board.legal_moves();

        assert!(!moves.contains(&Move {
            from: Square::from_str("e6").unwrap(),
            to: Square::from_str("d5").unwrap(),
            piece: PieceType::Pawn,
            capture: Some(PieceType::Rook),
            move_type: MoveType::Quiet
        }));
        assert_eq!(
            moves.iter().filter(|m| m.piece == PieceType::Pawn).count(),
            0
        );
        assert!(moves.iter().all(|m| m.piece == PieceType::King));
    }

    #[test]
    fn double_check() {
        let mut board = Board::from_fen("2nk4/8/8/8/7B/8/3R4/6K1 b - - 0 1").unwrap();
        let moves = board.legal_moves();

        assert!(moves.iter().all(|m| m.piece == PieceType::King));
    }

    #[test]
    fn check_whith_knight() {
        let mut board = Board::from_fen("8/3n4/1p4N1/8/1P5k/8/5K2/8 b - - 41 39").unwrap();
        let moves = board.legal_moves();

        assert!(board.king_in_check().0);
        assert!(moves.iter().all(|m| m.piece == PieceType::King));
    }

    #[test]
    fn castling_rules() {
        let mut white_both_side =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1").unwrap();
        let moves = white_both_side.legal_moves();

        let castling = white_both_side.get_available_castling();
        assert!(castling.is_some());
        assert_eq!(castling.unwrap(), CastlingSide::Both);
        let _ = white_both_side.make_move(
            *moves
                .iter()
                .find(|m| m.from == Square::from_str("a1").unwrap() && m.piece == PieceType::Rook)
                .unwrap(),
        ); // move rook
        let moves = white_both_side.legal_moves();
        let _ = white_both_side.make_move(moves[0]);

        let castling = white_both_side.get_available_castling();
        assert_eq!(castling.unwrap(), CastlingSide::KingSide);

        let moves = white_both_side.legal_moves();
        let _ = white_both_side.make_move(
            *moves
                .iter()
                .find(|m| m.from == Square::from_str("h1").unwrap() && m.piece == PieceType::Rook)
                .unwrap(),
        ); // move rook
        let moves = white_both_side.legal_moves();
        let _ = white_both_side.make_move(moves[0]);

        let castling = white_both_side.get_available_castling();
        assert!(castling.is_none());
    }

    #[test]
    fn castling_rules_black() {
        let mut black_both_side =
            Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b Kkq - 0 1").unwrap();
        let moves = black_both_side.legal_moves();

        let castling = black_both_side.get_available_castling();
        assert!(castling.is_some());
        assert_eq!(castling.unwrap(), CastlingSide::Both);
        let _ = black_both_side.make_move(
            *moves
                .iter()
                .find(|m| m.from == Square::from_str("a8").unwrap() && m.piece == PieceType::Rook)
                .unwrap(),
        ); // move rook
        let moves = black_both_side.legal_moves();
        let _ = black_both_side.make_move(moves[0]);

        let castling = black_both_side.get_available_castling();
        assert_eq!(castling.unwrap(), CastlingSide::KingSide);

        let moves = black_both_side.legal_moves();
        let _ = black_both_side.make_move(
            *moves
                .iter()
                .find(|m| m.from == Square::from_str("h8").unwrap() && m.piece == PieceType::Rook)
                .unwrap(),
        ); // move rook
        let moves = black_both_side.legal_moves();
        let _ = black_both_side.make_move(moves[0]);

        let castling = black_both_side.get_available_castling();
        assert!(castling.is_none());
    }

    #[test]
    fn check() {
        let mut board = Board::from_fen("4k3/3r4/8/8/8/8/3K4/5B2 w - - 0 1").unwrap();
        assert!(board.king_in_check().0);
        let moves = board.legal_moves();
        assert_eq!(moves.len(), 7);

        assert!(moves.iter().all(|m| m.piece == PieceType::King
            || (m.piece == PieceType::Bishop && m.to == Square::from_str("d3").unwrap())));
    }

    #[test]
    fn mate() {
        let mut board = Board::from_fen("3k4/2R1Q3/8/8/8/8/8/5K2 b - - 0 1").unwrap();
        assert!(board.king_in_check().0);
        let moves = board.legal_moves();
        assert!(moves.is_empty());
        assert_eq!(board.status, Status::Checkmate(Color::White));
    }

    #[test]
    fn stalemate() {
        let mut board = Board::from_fen("k7/2R5/8/8/1Q6/8/5p2/5K2 b - - 0 1").unwrap();
        assert!(!board.king_in_check().0);
        let moves = board.legal_moves();
        assert!(moves.is_empty());
        assert_eq!(board.status, Status::Stalemate);
    }

    #[test]
    fn castling_white() {
        let mut board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1").unwrap();
        let moves = board.legal_moves();
        let castle = moves
            .iter()
            .find(|m| {
                m.move_type
                    == MoveType::Castling {
                        side: CastlingSide::KingSide,
                    }
            })
            .unwrap()
            .to_owned();
        let king = board.white_pieces[PieceType::King as usize];
        assert_eq!(king.0.count_ones(), 1);
        assert_eq!(king.lsb_square(), Square::E1);
        assert!(king & board.white != 0);

        let rook = Square::H1;
        assert!(board.white_pieces[PieceType::Rook as usize] & Bitboard::from_square(rook) != 0);
        let _ = board.make_move(castle);

        let king = board.white_pieces[PieceType::King as usize];
        assert_eq!(king.0.count_ones(), 1);
        assert_eq!(king.lsb_square(), Square::G1);
        assert!(king & board.white != 0);
        assert!(
            board.white_pieces[PieceType::Rook as usize] & Bitboard::from_square(Square::F1) != 0
        );
    }

    #[test]
    fn castling_black() {
        let mut board =
            Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQq - 0 1").unwrap();
        let moves = board.legal_moves();
        let castle = moves
            .iter()
            .find(|m| {
                m.move_type
                    == MoveType::Castling {
                        side: CastlingSide::QueenSide,
                    }
            })
            .unwrap()
            .to_owned();
        let king = board.black_pieces[PieceType::King as usize];
        assert_eq!(king.0.count_ones(), 1);
        assert_eq!(king.lsb_square(), Square::E8);
        assert!(king & board.black != 0);

        let rook = Square::A8;
        assert!(board.black_pieces[PieceType::Rook as usize] & Bitboard::from_square(rook) != 0);
        let _ = board.make_move(castle);

        let king = board.black_pieces[PieceType::King as usize];
        assert_eq!(king.0.count_ones(), 1);
        assert_eq!(king.lsb_square(), Square::C8);
        assert!(king & board.black != 0);
        assert!(
            board.black_pieces[PieceType::Rook as usize] & Bitboard::from_square(Square::D8) != 0
        );
    }

    #[test]
    fn protected_pieces() {
        let mut board = Board::from_fen("1b4k1/8/8/8/5r2/4K3/8/8 w - - 0 1").unwrap();
        let moves = board.legal_moves();
        assert_eq!(moves.len(), 3);
        assert!(!moves
            .iter()
            .any(|m| m.capture.is_some() && m.piece == PieceType::King),);
    }
}
