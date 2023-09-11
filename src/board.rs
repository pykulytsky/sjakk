use std::{
    collections::{BTreeMap, VecDeque},
    sync::{
        atomic::{AtomicI32, AtomicU32, Ordering},
        Arc,
    },
};

use futures::executor::ThreadPool;
use rand::seq::SliceRandom;
use smallvec::{smallvec, SmallVec};
use thiserror::Error;

use crate::{
    constants::{self, MASK_RANK},
    gen_moves::{
        get_bishop_moves, get_bishop_rays, get_king_moves, get_knight_moves, get_pawn_attacks,
        get_rook_moves, get_rook_rays,
    },
    hashing,
    moves::{CastlingSide, Move, MoveList},
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
    pub en_passant_square: Option<Square>,
    hash: u64,
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
            en_passant_square: None,
            hash: 0,
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
    pub fn captured_piece(&self, m: &Move) -> Option<PieceType> {
        let pieces = self.pieces(self.side_to_move.opposite());
        PieceType::ALL
            .into_iter()
            .find(|piece| (pieces[piece.to_owned() as usize] & (1 << m.to().0)) != 0)
    }

    #[inline]
    pub fn moved_piece(&self, m: &Move) -> PieceType {
        let pieces = self.pieces(self.side_to_move);
        PieceType::ALL
            .into_iter()
            .find(|piece| (pieces[piece.to_owned() as usize] & (1 << m.from().0)) != 0)
            .unwrap()
    }

    #[inline]
    pub fn free(&self) -> Bitboard {
        !self.all_pieces()
    }

    #[inline]
    pub fn legal_moves(&self) -> MoveList {
        let (pinned_bb, checkers) = self.find_pinned();
        let (own_pieces, own_combined) = match self.side_to_move {
            Color::White => (self.white_pieces, self.white),
            Color::Black => (self.black_pieces, self.black),
        };
        let mut moves = MoveList::new();

        let ksq = own_pieces[PieceType::King as usize].lsb_square();

        let check_mask = if checkers.0.count_ones() == 1 {
            between(checkers.lsb_square(), ksq) ^ checkers
        } else {
            Bitboard::universe()
        };
        if checkers.0.count_ones() > 1 {
            let bb = PieceType::King.pseudo_legal_moves(
                ksq,
                self.side_to_move,
                self.all_pieces(),
                own_combined,
            );

            for sq in bb {
                if self.attacks_to(sq, self.side_to_move.opposite(), self.all_pieces()) == 0 {
                    moves.push(Move::new(ksq, sq));
                }
            }

            return moves;
        }
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
                if piece == PieceType::King {
                    for sq in bb {
                        if self.attacks_to(sq, self.side_to_move.opposite(), self.all_pieces()) == 0
                        {
                            moves.push(Move::new(ksq, sq));
                        }
                    }
                    continue;
                }
                if checkers.0.count_ones() == 1 && piece != PieceType::King {
                    bb &= check_mask;
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

        if checkers.0.count_ones() == 0 {
            self.castling_rules(&mut moves);
        }
        self.available_en_passant(&mut moves, checkers, check_mask, pinned_bb);
        moves
    }

    fn castling_rules(&self, moves: &mut MoveList) {
        match (self.side_to_move, self.available_castling()) {
            (Color::White, Some(castling)) => match castling {
                CastlingSide::KingSide => {
                    moves.push(Move::new_castle(Square::E1, Square::G1));
                }
                CastlingSide::QueenSide => {
                    moves.push(Move::new_castle(Square::E1, Square::C1));
                }
                CastlingSide::Both => {
                    moves.push(Move::new_castle(Square::E1, Square::G1));
                    moves.push(Move::new_castle(Square::E1, Square::C1));
                }
            },
            (Color::Black, Some(castling)) => match castling {
                CastlingSide::KingSide => {
                    moves.push(Move::new_castle(Square::E8, Square::G8));
                }
                CastlingSide::QueenSide => {
                    moves.push(Move::new_castle(Square::E8, Square::C8));
                }
                CastlingSide::Both => {
                    moves.push(Move::new_castle(Square::E8, Square::G8));
                    moves.push(Move::new_castle(Square::E8, Square::C8));
                }
            },
            _ => {}
        }
    }

    #[inline]
    fn fill_move_list(
        &self,
        move_list: &mut MoveList,
        sq: Square,
        moves: Bitboard,
        piece: PieceType,
    ) {
        for m in moves {
            if piece == PieceType::Pawn
                && ((m.rank() == Rank::Rank8 && self.side_to_move == Color::White)
                    || (m.rank() == Rank::Rank1 && self.side_to_move == Color::Black))
            {
                move_list.push(Move::new_promotion(sq, m, PieceType::Queen));
                move_list.push(Move::new_promotion(sq, m, PieceType::Rook));
                move_list.push(Move::new_promotion(sq, m, PieceType::Bishop));
                move_list.push(Move::new_promotion(sq, m, PieceType::Knight));
            } else {
                move_list.push(Move::new(sq, m));
            }
        }
    }

    #[inline]
    pub fn attacks_to(&self, square: Square, color: Color, occupied: Bitboard) -> Bitboard {
        let knights = self.pieces(color)[PieceType::Knight as usize];
        let king = self.pieces(color)[PieceType::King as usize];
        let pawns = self.pieces(color)[PieceType::Pawn as usize];
        let rooks_queens = self.pieces(color)[PieceType::Rook as usize]
            | self.pieces(color)[PieceType::Queen as usize];
        let bishops_queens = self.pieces(color)[PieceType::Bishop as usize]
            | self.pieces(color)[PieceType::Queen as usize];
        let ksq = self.pieces(color.opposite())[PieceType::King as usize];
        let pawns = get_pawn_attacks(square, color.opposite(), occupied ^ ksq) & pawns;
        let knights = get_knight_moves(square) & knights;
        let king = get_king_moves(square) & king;
        let rooks_queens = get_rook_moves(square, occupied ^ ksq) & rooks_queens;
        let bishops_queens = get_bishop_moves(square, occupied ^ ksq) & bishops_queens;
        pawns | knights | king | rooks_queens | bishops_queens
    }

    #[inline]
    pub fn is_attacked(&self, square: Square, color: Color, occupied: Bitboard) -> bool {
        let knights = self.pieces(color)[PieceType::Knight as usize];
        let king = self.pieces(color)[PieceType::King as usize];
        let pawns = self.pieces(color)[PieceType::Pawn as usize];
        let rooks_queens = self.pieces(color)[PieceType::Rook as usize]
            | self.pieces(color)[PieceType::Queen as usize];
        let bishops_queens = self.pieces(color)[PieceType::Bishop as usize]
            | self.pieces(color)[PieceType::Queen as usize];
        let ksq = self.pieces(color.opposite())[PieceType::King as usize];

        if get_pawn_attacks(square, color.opposite(), occupied ^ ksq) & pawns != 0 {
            return true;
        }
        if get_knight_moves(square) & knights != 0 {
            return true;
        }
        if get_king_moves(square) & king != 0 {
            return true;
        }
        if get_rook_moves(square, occupied ^ ksq) & rooks_queens != 0 {
            return true;
        }
        if get_bishop_moves(square, occupied ^ ksq) & bishops_queens != 0 {
            return true;
        }

        false
    }

    pub fn make_move(&mut self, m: &Move) -> Result<(), IllegalMove> {
        if self.legal_moves().inner.contains(m) {
            unsafe { self.make_move_unchecked(m) }
            return Ok(());
        }

        Err(IllegalMove)
    }

    pub fn make_move_new(&mut self, m: &Move) -> Board {
        let mut board = self.clone();
        unsafe { board.make_move_unchecked(m) }
        board
    }

    /// Updates all the bitboards, which are involved in move, updates side to move and moves list.
    ///
    /// # Safety
    /// This method does not check if provided move is a valid move, it may break representation of
    /// the game. Use it only with moves, received from [`Self::pseudo_legal_moves`]. Otherwise use [`Self::make_move`].
    pub unsafe fn make_move_unchecked(&mut self, m: &Move) {
        let piece = self.moved_piece(m);
        let captured = self.captured_piece(m);

        let piece_idx = if self.side_to_move == Color::White {
            piece as usize
        } else {
            piece as usize * 2
        };
        self.hash ^= hashing::PIECE_KEYS[piece_idx][m.from().0 as usize];
        self.hash ^= hashing::PIECE_KEYS[piece_idx][m.to().0 as usize];
        if let Some(captured) = captured {
            let piece_idx = if self.side_to_move == Color::White {
                captured as usize
            } else {
                captured as usize * 2
            };
            self.hash ^= hashing::PIECE_KEYS[piece_idx][m.to().0 as usize]
        }

        if piece == PieceType::Pawn {
            self.halfmoves = 0;
        }
        if captured.is_none() && piece != PieceType::Pawn {
            self.halfmoves += 1;

            if self.halfmoves == 100 {
                self.status = Status::Draw(DrawReason::Halfmoves);
            }
        }
        // update castling rights
        match piece {
            PieceType::Rook => match m.from() {
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
            PieceType::King => match m.from() {
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

        if let Some(PieceType::Rook) = captured {
            match m.to() {
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
        if piece == PieceType::Pawn
            && ((m.from().rank() == Rank::Rank2 && m.to().rank() == Rank::Rank4)
                || (m.from().rank() == Rank::Rank7 && m.to().rank() == Rank::Rank5))
        {
            let square = if self.side_to_move == Color::White {
                Square(m.to().0 - 8)
            } else {
                Square(m.to().0 + 8)
            };
            self.en_passant_square = Some(square);
            self.hash ^= hashing::EP_KEYS[square.file() as usize];
        } else if self.en_passant_square.is_some() {
            let square = self.en_passant_square.unwrap();
            self.hash ^= hashing::EP_KEYS[square.file() as usize];
            self.en_passant_square = None;
        }
        self.update_position(m);
        self.side_to_move = self.side_to_move.opposite();
        self.hash ^= hashing::SIDE_KEY;
        self.move_list.push(*m);
    }

    #[inline]
    fn update_position(&mut self, m: &Move) {
        let piece = self.moved_piece(m);
        let capture = self.captured_piece(m);
        let from_bb = Bitboard::from_square(m.from());
        let to_bb = Bitboard::from_square(m.to());
        let from_to_bb = from_bb ^ to_bb;
        let own_pieces = match self.side_to_move {
            Color::White => &mut self.white_pieces,
            Color::Black => &mut self.black_pieces,
        };
        let own = match self.side_to_move {
            Color::White => &mut self.white,
            Color::Black => &mut self.black,
        };

        if m.is_en_passant() {
            own_pieces[piece as usize] ^= from_to_bb;
            *own ^= from_to_bb;
            let target = m.en_passant_target().unwrap();
            let target_bb = Bitboard::from_square(target);
            match self.side_to_move.opposite() {
                Color::White => {
                    self.white_pieces[PieceType::Pawn as usize] ^= target_bb;
                    self.white ^= target_bb;
                }
                Color::Black => {
                    self.black_pieces[PieceType::Pawn as usize] ^= target_bb;
                    self.black ^= target_bb;
                }
            }
        } else if m.is_promotion() {
            let promotion = m.promotion_to().unwrap();
            own_pieces[piece as usize] ^= from_bb;
            own_pieces[promotion as usize] ^= to_bb;
            *own ^= from_to_bb;
        } else if m.is_castle() {
            own_pieces[piece as usize] ^= from_to_bb;
            *own ^= from_to_bb;
            let rook = match (self.side_to_move, m.to()) {
                (Color::White, Square::C1) => {
                    Bitboard::from_square(Square::A1) ^ Bitboard::from_square(Square::D1)
                }
                (Color::White, Square::G1) => {
                    Bitboard::from_square(Square::H1) ^ Bitboard::from_square(Square::F1)
                }
                (Color::Black, Square::C8) => {
                    Bitboard::from_square(Square::A8) ^ Bitboard::from_square(Square::D8)
                }
                (Color::Black, Square::G8) => {
                    Bitboard::from_square(Square::H8) ^ Bitboard::from_square(Square::F8)
                }
                _ => unreachable!(),
            };
            own_pieces[PieceType::Rook as usize] ^= rook;
            *own ^= rook;
        } else {
            own_pieces[piece as usize] ^= from_to_bb;
            *own ^= from_to_bb;
        }

        if let Some(capture) = capture {
            match self.side_to_move.opposite() {
                Color::White => {
                    self.white_pieces[capture as usize] ^= to_bb;
                    self.white ^= to_bb;
                }
                Color::Black => {
                    self.black_pieces[capture as usize] ^= to_bb;
                    self.black ^= to_bb;
                }
            }
        }
    }

    /// Returns pinned ray, for given square, if piece on this square is pinned.
    /// This method checks only absolute pins, since only absolute pins are required for legal move
    /// generation.
    #[inline]
    pub fn pinned(&self, square: Square) -> Option<Bitboard> {
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

    pub fn legal_ep_move(&self, source: Square, dest: Square, ep_square: Square) -> bool {
        let combined = self.all_pieces()
            ^ Bitboard::from_square(ep_square)
            ^ Bitboard::from_square(source)
            ^ Bitboard::from_square(dest);

        let ksq = self.pieces(self.side_to_move)[PieceType::King as usize].lsb_square();

        let color = self.side_to_move.opposite();
        let rooks = self.pieces(color)[PieceType::Rook as usize]
            | self.pieces(color)[PieceType::Queen as usize];
        let bishops = self.pieces(color)[PieceType::Bishop as usize]
            | self.pieces(color)[PieceType::Queen as usize];

        if (get_rook_rays(ksq) & rooks).0 != 0 && (get_rook_moves(ksq, combined) & rooks).0 != 0 {
            return false;
        }

        if (get_bishop_rays(ksq) & bishops).0 != 0
            && (get_bishop_moves(ksq, combined) & bishops).0 != 0
        {
            return false;
        }

        true
    }
    #[inline]
    pub fn available_en_passant(
        &self,
        moves: &mut MoveList,
        checkers: Bitboard,
        check_mask: Bitboard,
        pinned_bb: Bitboard,
    ) {
        if let Some(target_square) = self.en_passant_square {
            if checkers.0.count_ones() == 1
                && Bitboard::from_square(target_square) & check_mask == 0
            {
                return;
            }
            let color = match target_square.rank() {
                Rank::Rank3 => Color::White,
                Rank::Rank6 => Color::Black,
                _ => unreachable!(),
            };
            let targets = Pawn::pawn_attacks(color, target_square);
            let targets = targets & self.pieces(self.side_to_move)[PieceType::Pawn as usize];
            for target in targets {
                let pinned = pinned_bb & Bitboard::from_square(target) != 0;
                let king_square =
                    self.pieces(self.side_to_move)[PieceType::King as usize].lsb_square();
                if pinned {
                    let pin = self.get_ray(king_square, target);
                    if pin & Bitboard::from_square(target_square) == 0 {
                        continue;
                    }
                }
                if king_square.rank() == target.rank() {
                    let mut queens_rooks = self.pieces(self.side_to_move.opposite())
                        [PieceType::Rook as usize]
                        | self.pieces(self.side_to_move.opposite())[PieceType::Queen as usize];
                    if MASK_RANK[king_square.rank() as usize] & queens_rooks != 0
                        && queens_rooks.any(|attacker| {
                            let attacks = get_rook_moves(
                                attacker,
                                self.all_pieces()
                                    ^ Bitboard::from_square(target)
                                    ^ Bitboard::from_square(Square::from_file_and_rank(
                                        target_square.file(),
                                        target.rank(),
                                    )),
                            );
                            attacks & Bitboard::from_square(king_square) != 0
                        })
                    {
                        continue;
                    }
                }
                moves.push(Move::new_en_passant(target, target_square))
            }
        }
    }

    pub fn set_status(&mut self, num_of_legal_moves: usize, king_in_check: bool) {
        match (king_in_check, num_of_legal_moves) {
            (true, 0) => self.status = Status::Checkmate(self.side_to_move.opposite()),
            (false, 0) => self.status = Status::Stalemate,
            _ => {}
        }
    }

    /// Returns [`Bitboard`] with pinned pieces and [`Bitboard`] with checkers.
    #[inline]
    pub fn find_pinned(&self) -> (Bitboard, Bitboard) {
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
        let mut checkers = Bitboard(0);
        for sq in attackers {
            let between = between(sq, king) & self.all_pieces();
            if between == 0 {
                checkers ^= Bitboard::from_square(sq);
            } else if between.0.count_ones() == 1 {
                pinned ^= between;
            }
        }
        let knight_moves =
            PieceType::Knight.pseudo_legal_moves(king, self.side_to_move, Bitboard(0), Bitboard(0))
                & self.pieces_combined(self.side_to_move.opposite())
                & self.pieces(self.side_to_move.opposite())[PieceType::Knight as usize];
        // let mut pawn_attacks = Bitboard(0);
        // for pawn in self.pieces(self.side_to_move.opposite())[PieceType::Pawn as usize] {
        //     if Bitboard::from_square(king) & Pawn::pawn_attacks(self.side_to_move.opposite(), pawn)
        //         != 0
        //     {
        //         checkers ^= Bitboard::from_square(pawn);
        //     }
        // }
        let pawn_attacks = Pawn::pawn_attacks(self.side_to_move, king)
            & self.pieces(self.side_to_move.opposite())[PieceType::Pawn as usize];
        checkers ^= knight_moves;
        checkers ^= pawn_attacks;

        (pinned, checkers)
    }

    #[inline]
    fn get_ray(&self, sq1: Square, sq2: Square) -> Bitboard {
        Bitboard(
            RAY_ATTACKS[sq1.0 as usize]
                .into_iter()
                .find(|r| r & Bitboard::from_square(sq2).0 != 0)
                .unwrap_or_else(|| {
                    panic!("ray");
                }),
        )
    }

    pub fn get_available_castling(&self) -> Option<CastlingSide> {
        self.available_castling()
    }

    #[inline]
    fn available_castling(&self) -> Option<CastlingSide> {
        let king_on_original_square = !self.castling_rights.king_moved(self.side_to_move);
        if !king_on_original_square {
            return None;
        }
        let a_rook_on_original_square = !self.castling_rights.a_rook_moved(self.side_to_move);
        let h_rook_on_original_square = !self.castling_rights.h_rook_moved(self.side_to_move);

        if !a_rook_on_original_square && !h_rook_on_original_square {
            return None;
        }

        let mut queen_side_castling_is_attacked = false;
        let mut king_side_castling_is_attacked = false;
        if self.side_to_move == Color::White {
            for sq in [Square::C1, Square::D1] {
                if self.is_attacked(sq, Color::Black, self.all_pieces()) {
                    queen_side_castling_is_attacked = true;
                    break;
                }
            }
            for sq in [Square::F1, Square::G1] {
                if self.is_attacked(sq, Color::Black, self.all_pieces()) {
                    king_side_castling_is_attacked = true;
                    break;
                }
            }
        } else {
            for sq in [Square::C8, Square::D8] {
                if self.is_attacked(sq, Color::White, self.all_pieces()) {
                    queen_side_castling_is_attacked = true;
                    break;
                }
            }
            for sq in [Square::F8, Square::G8] {
                if self.is_attacked(sq, Color::White, self.all_pieces()) {
                    king_side_castling_is_attacked = true;
                    break;
                }
            }
        }
        let pieces = self.all_pieces();
        let king_side = if !king_side_castling_is_attacked {
            (match self.side_to_move {
                Color::White => (pieces.0 >> 5).trailing_zeros() == 2,
                Color::Black => (pieces.0.swap_bytes() >> 5).trailing_zeros() == 2,
            }) && h_rook_on_original_square
        } else {
            false
        };
        let queen_side = if !queen_side_castling_is_attacked {
            (match self.side_to_move {
                Color::White => (pieces.0 >> 1).trailing_zeros() == 3,
                Color::Black => (pieces.0.swap_bytes() >> 1).trailing_zeros() == 3,
            }) && a_rook_on_original_square
        } else {
            false
        };

        match (king_on_original_square, king_side, queen_side) {
            (true, true, true) => Some(CastlingSide::Both),
            (true, true, false) => Some(CastlingSide::KingSide),
            (true, false, true) => Some(CastlingSide::QueenSide),
            (true, false, false) => None,
            (false, _, _) => None,
        }
    }

    #[inline]
    pub fn material(&self) -> f32 {
        PieceType::ALL
            .into_iter()
            .map(|p| {
                p.value() as f32
                    * (self.pieces(Color::White)[p as usize].0.count_ones() as f32
                        - self.pieces(Color::Black)[p as usize].0.count_ones() as f32)
            })
            .reduce(|acc, m| acc + m)
            .unwrap()
    }

    #[inline]
    pub fn mobility(&mut self) -> f32 {
        let original_side = self.side_to_move;
        self.side_to_move = Color::White;
        let white_moves = self.legal_moves();
        self.side_to_move = Color::Black;
        let black_moves = self.legal_moves();
        self.side_to_move = original_side;

        (white_moves.len() as f32 - black_moves.len() as f32) * 0.2
    }

    #[inline]
    pub fn evaluate(&mut self) -> f32 {
        self.material()
    }

    #[inline]
    pub fn evaluate_relative(&mut self) -> f32 {
        self.evaluate() * (self.side_to_move.eval_mask() as f32)
    }

    #[inline]
    pub fn negamax(&mut self, depth: usize) -> f32 {
        if depth == 0 {
            return self.evaluate_relative();
        }
        let mut max = -f32::MAX;
        let moves = self.legal_moves();
        for m in moves.iter() {
            let mut board = self.make_move_new(m);
            let score = -board.negamax(depth - 1);
            if score > max {
                max = score;
            }
        }
        max
    }

    #[inline]
    pub fn alpha_beta_negamax(&mut self, mut alpha: f32, beta: f32, depth: usize) -> f32 {
        if depth == 0 {
            return self.evaluate_relative();
        }

        let moves = self.legal_moves();
        for m in moves.iter() {
            let mut board = self.make_move_new(m);
            let score = -board.alpha_beta_negamax(-beta, -alpha, depth - 1);

            if score >= beta {
                return score;
            }
            if score > alpha {
                alpha = score;
            }
        }
        alpha
    }

    #[inline]
    pub fn alpha_beta_negamax_root(&mut self, depth: usize) -> (f32, Option<Move>) {
        if depth == 0 {
            return (f32::MIN, None);
        }
        let mut alpha = f32::MIN;
        let mut alpha_move = None;
        let beta = f32::MAX;
        let moves = self.legal_moves();
        for m in moves.iter() {
            let mut board = self.make_move_new(m);
            let score = -board.alpha_beta_negamax(-beta, -alpha, depth - 1);
            if score >= beta {
                return (beta, Some(m.to_owned()));
            }
            if score > alpha {
                alpha = score;
                alpha_move = Some(m.to_owned());
            }
        }
        (alpha, alpha_move)
    }

    #[inline]
    pub fn alpha_beta_negamax_root_async(
        &mut self,
        depth: usize,
        executor: &ThreadPool,
    ) -> (f32, Option<Move>) {
        let moves = self.legal_moves();
        if moves.is_empty() {
            return (f32::MIN, None);
        }
        let mut results = VecDeque::new();
        let move_number = Arc::new(AtomicU32::new(0));
        let barrier = Arc::new(AtomicU32::new(moves.len() as u32));
        let alpha = Arc::new(AtomicI32::new(i32::MIN));
        let beta = Arc::new(f32::MAX);
        for (i, m) in moves.iter().enumerate() {
            let m = m.clone().to_owned();
            let barrier = barrier.clone();
            let move_number = move_number.clone();
            let mut b = self.clone();
            let alpha = alpha.clone();
            let beta = beta.clone();
            results.push_back(async move {
                let result = async move {
                    let mut board = b.make_move_new(&m);
                    let score = -board.alpha_beta_negamax(
                        -(*beta),
                        -(alpha.load(Ordering::Acquire) as f32 / 1000.0),
                        depth - 1,
                    );
                    if score >= *beta {
                        alpha.store((*beta * 1000.0) as i32, Ordering::Release);
                        move_number.store(i as u32, Ordering::Release);
                        return;
                    }
                    if score > alpha.load(Ordering::Acquire) as f32 {
                        alpha.store((score * 1000.0) as i32, Ordering::Release);
                        move_number.store(i as u32, Ordering::Release);
                    }
                    drop(alpha);
                    barrier.fetch_sub(1, Ordering::Release);
                };
                executor.spawn_ok(result);
            });
        }
        for res in results {
            futures::executor::block_on(res);
        }

        while barrier.load(Ordering::Acquire) != 0 {
            std::hint::spin_loop();
        }
        (
            alpha.load(Ordering::Acquire) as f32 / 1000.0,
            Some(moves[move_number.load(Ordering::Acquire) as usize]),
        )
    }

    #[inline]
    pub fn negamax_root(&mut self, depth: usize) -> (f32, Option<Move>) {
        let mut max = -f32::MAX;
        let moves = self.legal_moves();
        if moves.is_empty() {
            return (max, None);
        }
        let mut mv = moves.inner.get(0).copied();
        for m in moves.iter() {
            let mut board = self.make_move_new(m);
            let score = -board.negamax(depth - 1);
            if score > max {
                max = score;
                mv = Some(m.to_owned());
            }
        }
        (max, mv)
    }

    #[inline]
    pub fn negamax_root_async(
        &mut self,
        depth: usize,
        executor: &ThreadPool,
    ) -> (i32, Option<Move>) {
        let moves = self.legal_moves();
        if moves.is_empty() {
            return (i32::MIN, None);
        }
        let mut results = VecDeque::new();
        let sc = Arc::new(AtomicI32::new(i32::MIN));
        let move_number = Arc::new(AtomicU32::new(0));
        let barrier = Arc::new(AtomicU32::new(moves.len() as u32));
        for (i, m) in moves.iter().enumerate() {
            let m = m.clone().to_owned();
            let sc = sc.clone();
            let barrier = barrier.clone();
            let move_number = move_number.clone();
            let mut b = self.clone();
            results.push_back(async move {
                let result = async move {
                    let mut board = b.make_move_new(&m);
                    let score = -board.negamax(depth - 1);
                    if (score * 1000.0) as i32 > sc.load(Ordering::Acquire) {
                        sc.store((score * 1000.0) as i32, Ordering::Release);
                        move_number.store(i as u32, Ordering::Release);
                    }
                    barrier.fetch_sub(1, Ordering::Release);
                };
                executor.spawn_ok(result);
            });
        }
        for res in results {
            futures::executor::block_on(res);
        }

        while barrier.load(Ordering::Acquire) != 0 {
            std::hint::spin_loop();
        }

        (
            sc.load(Ordering::Acquire),
            Some(moves[move_number.load(Ordering::Acquire) as usize]),
        )
    }
}

#[inline]
pub fn perft(board: &mut Board, depth: usize) -> usize {
    let moves = board.legal_moves();

    let mut final_depth = 0;

    if depth == 1 {
        moves.len()
    } else {
        for m in moves.iter() {
            let mut board = board.make_move_new(m);
            final_depth += perft(&mut board, depth - 1);
        }

        final_depth
    }
}

#[inline]
pub fn perft_async(board: &Board, depth: usize, executor: &ThreadPool) -> usize {
    let moves = board.legal_moves();

    let final_depth = Arc::new(AtomicU32::new(0));
    let mut results = VecDeque::new();
    let barrier = Arc::new(AtomicU32::new(moves.len() as u32));

    if depth == 1 {
        moves.len()
    } else {
        for m in moves.iter() {
            let m = *m;
            let final_depth = final_depth.clone();
            let mut board = board.clone();
            let barrier = barrier.clone();
            results.push_back(async move {
                let result = async move {
                    let mut board = board.make_move_new(&m);
                    let nodes = perft(&mut board, depth - 1);
                    final_depth.fetch_add(nodes as u32, Ordering::Release);
                    barrier.fetch_sub(1, Ordering::Release);
                };

                executor.spawn_ok(result);
            });
        }

        for res in results {
            futures::executor::block_on(res);
        }

        while barrier.load(Ordering::Acquire) != 0 {
            std::hint::spin_loop();
        }

        final_depth.load(Ordering::Acquire) as usize
    }
}

#[inline]
pub fn perft_detailed(board: &mut Board, depth: usize) -> (BTreeMap<Move, usize>, usize) {
    let moves = board.legal_moves();

    let mut nodes = BTreeMap::new();
    let mut final_nodes = 0;

    for m in moves.iter() {
        let mut board = board.make_move_new(m);
        let moves = perft(&mut board, depth - 1);
        final_nodes += moves;
        nodes.insert(m.to_owned(), moves);
    }

    (nodes, final_nodes)
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
            en_passant_square: None,
            hash: 0,
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
                    move_list.push(Move::new(from_square, to_square));
                }
                // Black pawn moved 2 squares up.
                Rank::Rank6 => {
                    let from_square = Square::from_file_and_rank(square.file(), Rank::Rank7);
                    let to_square = Square::from_file_and_rank(square.file(), Rank::Rank5);
                    move_list.push(Move::new(from_square, to_square));
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
            en_passant_square: fen.en_passant_target,
            hash: 0,
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
        if moves.is_empty() {
            self.board.status = Status::Checkmate(self.board.side_to_move.opposite());
            return None;
        }
        if self.board.status != Status::Ongoing || self.board.move_list.len() == 250 {
            println!("{:?}", self.board.status);
            return None;
        }

        let mut rng = rand::thread_rng();
        let m = if let Some(m) = moves.iter().find(|m| m.is_castle()) {
            m.to_owned()
        } else {
            let captures: Vec<&Move> = moves
                .iter()
                .filter(|m| self.board.captured_piece(m).is_some())
                .collect();
            if !captures.is_empty() {
                captures.choose(&mut rng).unwrap().to_owned().to_owned()
            } else {
                *moves.inner[..moves.len()].choose(&mut rng).unwrap()
            }
        };
        unsafe {
            self.board.make_move_unchecked(&m);
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
        let piece = board.captured_piece(&board.move_list[0]).unwrap();
        assert_eq!(piece, PieceType::Pawn);
        assert_eq!(board.move_list[0].from(), Square::from_str("d7").unwrap());
        assert_eq!(board.move_list[0].to(), Square::from_str("d5").unwrap());
        let moves = board.legal_moves();
        assert!(moves.iter().any(|m| m.is_en_passant()));
    }

    #[test]
    fn en_passant_under_pin() {
        let board = Board::from_fen("3kq3/8/8/4Pp2/8/8/8/4K3 w - f6 0 1").unwrap();
        let moves = board.legal_moves();

        assert_eq!(moves.iter().filter(|m| m.is_en_passant()).count(), 0);
        assert!(board.pinned(Square::from_str("e5").unwrap()).is_some());
        assert!(!moves.iter().any(|m| m.is_en_passant()));
    }

    #[test]
    fn en_passant_under_pin_in_ray() {
        let board = Board::from_fen("2k4b/8/8/4Pp2/8/8/1K6/8 w - f6 0 1").unwrap();
        let moves = board.legal_moves();

        assert_ne!(moves.iter().filter(|m| m.is_en_passant()).count(), 0);
        assert!(board.pinned(Square::from_str("e5").unwrap()).is_some());
        assert!(moves.iter().any(|m| m.is_en_passant()));
    }

    #[test]
    fn en_passant_under_pin_target_square_in_ray() {
        let board = Board::from_fen("8/8/1K5q/3pP3/8/8/8/7k w - d6 0 1").unwrap();
        let moves = board.legal_moves();

        assert_ne!(moves.iter().filter(|m| m.is_en_passant()).count(), 0);
        assert!(moves.iter().any(|m| m.is_en_passant()));
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
        let _ = board.make_move(&moves[0]);
        let moves = board.legal_moves();
        assert_eq!(moves.len(), 20);
        assert_eq!(board.side_to_move, Color::Black);
    }

    #[test]
    fn pin() {
        let board = Board::from_fen("4k3/8/7b/8/8/4P3/3K4/8 w - - 0 1").unwrap();
        let moves = board.legal_moves();

        assert!(board.pinned(Square::from_str("e3").unwrap()).is_some());
        assert_eq!(
            moves
                .iter()
                .filter(|m| {
                    let piece = board.moved_piece(m);
                    piece == PieceType::Pawn
                })
                .count(),
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
        let board = Board::from_fen("8/3k4/4p3/3R4/8/7B/8/6K1 b - - 0 1").unwrap();
        let moves = board.legal_moves();

        assert!(!moves.inner.contains(&Move::new(
            Square::from_str("e6").unwrap(),
            Square::from_str("d5").unwrap(),
        )));
        assert_eq!(
            moves
                .iter()
                .filter(|m| {
                    let piece = board.moved_piece(m);
                    piece == PieceType::Pawn
                })
                .count(),
            0
        );
        assert!(moves.iter().all(|m| {
            let piece = board.moved_piece(m);
            piece == PieceType::King
        }));
    }

    #[test]
    fn double_check() {
        let board = Board::from_fen("2nk4/8/8/8/7B/8/3R4/6K1 b - - 0 1").unwrap();
        let moves = board.legal_moves();

        assert!(moves.iter().all(|m| {
            let piece = board.moved_piece(m);
            piece == PieceType::King
        }));
    }

    #[test]
    fn check_whith_knight() {
        let board = Board::from_fen("8/3n4/1p4N1/8/1P5k/8/5K2/8 b - - 41 39").unwrap();
        let moves = board.legal_moves();

        assert!(board.find_pinned().1 .0.count_ones() == 1);
        assert!(moves.iter().all(|m| {
            let piece = board.moved_piece(m);
            piece == PieceType::King
        }));
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
            moves
                .iter()
                .find(|m| {
                    m.from() == Square::from_str("a1").unwrap() && {
                        let piece = white_both_side.moved_piece(m);
                        piece == PieceType::Rook
                    }
                })
                .unwrap(),
        ); // move rook
        let moves = white_both_side.legal_moves();
        let _ = white_both_side.make_move(&moves[0]);

        let castling = white_both_side.get_available_castling();
        assert_eq!(castling.unwrap(), CastlingSide::KingSide);

        let moves = white_both_side.legal_moves();
        let _ = white_both_side.make_move(
            moves
                .iter()
                .find(|m| {
                    m.from() == Square::from_str("h1").unwrap() && {
                        let piece = white_both_side.moved_piece(m);
                        piece == PieceType::Rook
                    }
                })
                .unwrap(),
        ); // move rook
        let moves = white_both_side.legal_moves();
        let _ = white_both_side.make_move(&moves[0]);

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
            moves
                .iter()
                .find(|m| {
                    m.from() == Square::from_str("a8").unwrap() && {
                        let piece = black_both_side.moved_piece(m);
                        piece == PieceType::Rook
                    }
                })
                .unwrap(),
        ); // move rook
        let moves = black_both_side.legal_moves();
        let _ = black_both_side.make_move(&moves[0]);

        let castling = black_both_side.get_available_castling();
        assert_eq!(castling.unwrap(), CastlingSide::KingSide);

        let moves = black_both_side.legal_moves();
        let _ = black_both_side.make_move(
            moves
                .iter()
                .find(|m| {
                    m.from() == Square::from_str("h8").unwrap() && {
                        let piece = black_both_side.moved_piece(m);
                        piece == PieceType::Rook
                    }
                })
                .unwrap(),
        ); // move rook
        let moves = black_both_side.legal_moves();
        let _ = black_both_side.make_move(&moves[0]);

        let castling = black_both_side.get_available_castling();
        assert!(castling.is_none());
    }

    #[test]
    fn check() {
        let board = Board::from_fen("4k3/3r4/8/8/8/8/3K4/5B2 w - - 0 1").unwrap();
        assert!(board.find_pinned().1 .0.count_ones() == 1);
        let moves = board.legal_moves();
        assert_eq!(moves.len(), 7);

        // assert!(moves.iter().all(|m| m.piece == PieceType::King
        //     || (m.piece == PieceType::Bishop && m.to == Square::from_str("d3").unwrap())));
    }

    #[test]
    fn mate() {
        let board = Board::from_fen("3k4/2R1Q3/8/8/8/8/8/5K2 b - - 0 1").unwrap();
        assert!(board.find_pinned().1 .0.count_ones() == 1);
        let moves = board.legal_moves();
        assert!(moves.is_empty());
        assert_eq!(board.find_pinned().1 .0.count_ones(), 1);
    }

    #[test]
    fn stalemate() {
        let board = Board::from_fen("k7/2R5/8/8/1Q6/8/5p2/5K2 b - - 0 1").unwrap();
        assert!(board.find_pinned().1 .0.count_ones() == 0);
        let moves = board.legal_moves();
        assert!(moves.is_empty());
        assert_eq!(board.find_pinned().1 .0.count_ones(), 0);
    }

    #[test]
    fn castling_white() {
        let mut board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1").unwrap();
        let moves = board.legal_moves();
        let castle = moves.iter().find(|m| m.is_castle()).unwrap().to_owned();
        let king = board.white_pieces[PieceType::King as usize];
        assert_eq!(king.0.count_ones(), 1);
        assert_eq!(king.lsb_square(), Square::E1);
        assert!(king & board.white != 0);

        let rook = Square::H1;
        assert!(board.white_pieces[PieceType::Rook as usize] & Bitboard::from_square(rook) != 0);
        let _ = board.make_move(&castle);

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
        let castle = moves.iter().find(|m| m.is_castle()).unwrap().to_owned();
        let king = board.black_pieces[PieceType::King as usize];
        assert_eq!(king.0.count_ones(), 1);
        assert_eq!(king.lsb_square(), Square::E8);
        assert!(king & board.black != 0);

        let rook = Square::A8;
        assert!(board.black_pieces[PieceType::Rook as usize] & Bitboard::from_square(rook) != 0);
        let _ = board.make_move(&castle);

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
        let board = Board::from_fen("1b4k1/8/8/8/5r2/4K3/8/8 w - - 0 1").unwrap();
        let moves = board.legal_moves();
        assert_eq!(moves.len(), 3);
        assert!(!moves.iter().any(|m| {
            let piece = board.moved_piece(m);
            let capture = board.captured_piece(m);
            capture.is_some() && piece == PieceType::King
        }));
    }

    #[test]
    fn perft_starting_position_depth_1() {
        let mut board = Board::default();
        assert_eq!(perft(&mut board, 1), 20);
    }

    #[test]
    fn perft_starting_position_depth_2() {
        let mut board = Board::default();
        assert_eq!(perft(&mut board, 2), 400);
    }

    #[test]
    fn perft_starting_position_depth_3() {
        let mut board = Board::default();
        assert_eq!(perft(&mut board, 3), 8902);
    }

    #[test]
    fn perft_starting_position_depth_4() {
        let mut board = Board::default();
        assert_eq!(perft(&mut board, 4), 197_281);
    }

    #[test]
    fn perft_starting_position_depth_5() {
        let mut board = Board::default();
        assert_eq!(perft(&mut board, 5), 4_865_609);
    }

    #[test]
    fn perft_depth_6() {
        let mut board = Board::from_fen("5K2/8/1Q6/2N5/8/1p2k3/8/8 w - - 0 1").unwrap();
        assert_eq!(perft(&mut board, 5), 1004658);
    }

    #[test]
    fn perft_kiwipete_depth_1() {
        let mut board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&mut board, 1), 48);
    }

    #[test]
    fn perft_kiwipete_depth_2() {
        let mut board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&mut board, 2), 2039);
    }

    #[test]
    fn perft_kiwipete_depth_3() {
        let mut board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&mut board, 3), 97862);
    }

    #[test]
    fn perft_kiwipete_depth_4() {
        let mut board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&mut board, 4), 4085603);
    }

    #[test]
    fn position_3_depth_1() {
        let mut board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
        assert_eq!(perft(&mut board, 1), 14);
    }

    #[test]
    fn position_3_depth_2() {
        let mut board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
        assert_eq!(perft(&mut board, 2), 191);
    }

    #[test]
    fn moved_piece() {
        let board = Board::default();
        let m = Move::new(Square(8), Square(16));
        assert_eq!(board.moved_piece(&m), PieceType::Pawn);
        assert_eq!(board.captured_piece(&m), None);
    }

    #[test]
    fn captured_piece() {
        let board = Board::from_fen("5K1n/8/8/8/8/8/3k4/B7 w - - 0 1").unwrap();
        let m = Move::new(Square(0), Square(63));
        assert_eq!(board.moved_piece(&m), PieceType::Bishop);
        assert_eq!(board.captured_piece(&m), Some(PieceType::Knight));
    }
}
