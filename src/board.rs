use std::sync::{Arc, Mutex};

use rand::seq::SliceRandom;
use thiserror::Error;

use crate::{
    castling_rights::CastlingRights,
    constants::{self, MASK_RANK},
    gen_moves::{
        get_bishop_moves, get_bishop_rays, get_king_moves, get_knight_moves, get_pawn_attacks,
        get_rook_moves, get_rook_rays,
    },
    hashing,
    moves::{CastlingSide, Move, MoveList},
    parsers::fen::{self, FENParseError, FEN},
    piece::{Bishop, Color, King, Knight, Pawn, PieceType, Queen, Rook},
    rays::{BISHOP_ATTACKS, RAY_ATTACKS, ROOK_ATTACKS},
    transposition_table::TranspositionTable,
    utils::between,
    Bitboard, Piece, Rank, Square,
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

#[derive(Debug, Clone)]
pub struct Board {
    pub white_pieces: [Bitboard; 6],
    pub black_pieces: [Bitboard; 6],
    pub white: Bitboard,
    pub black: Bitboard,
    pub halfmoves: u16,
    pub side_to_move: Color,
    pub status: Status,
    pub castling_rights: [CastlingRights; 2],
    pub en_passant_square: Option<Square>,
    pub ksq: Square,
    pub hash: u64,
    pub tt: Arc<Mutex<TranspositionTable>>,
}

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Illegal move")]
pub struct IllegalMove;

impl Board {
    pub fn new() -> Self {
        Board::default()
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

    #[inline]
    pub fn pieces(&self, color: Color) -> [Bitboard; 6] {
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

    pub fn init_hash(&mut self) {
        self.hash_piece_set(PieceType::Pawn, Color::White);
        self.hash_piece_set(PieceType::Rook, Color::White);
        self.hash_piece_set(PieceType::Bishop, Color::White);
        self.hash_piece_set(PieceType::Knight, Color::White);
        self.hash_piece_set(PieceType::Queen, Color::White);
        self.hash_piece_set(PieceType::King, Color::White);
        self.hash_piece_set(PieceType::Pawn, Color::Black);
        self.hash_piece_set(PieceType::Rook, Color::Black);
        self.hash_piece_set(PieceType::Bishop, Color::Black);
        self.hash_piece_set(PieceType::Knight, Color::Black);
        self.hash_piece_set(PieceType::Queen, Color::Black);
        self.hash_piece_set(PieceType::King, Color::Black);

        if let Some(target) = self.en_passant_square {
            self.hash ^= hashing::EP_KEYS[target.file() as usize];
        }
        self.hash ^= hashing::CASTLE_KEYS[0][self.castling_rights[0] as usize];
        self.hash ^= hashing::CASTLE_KEYS[1][self.castling_rights[1] as usize];

        if self.side_to_move == Color::Black {
            self.hash ^= hashing::SIDE_KEY;
        }
    }

    fn hash_piece_set(&mut self, piece: PieceType, color: Color) {
        let piece_idx = if self.side_to_move == Color::White {
            piece as usize
        } else {
            piece as usize * 2
        };
        for sq in self.pieces(color)[piece as usize] {
            self.hash ^= hashing::PIECE_KEYS[piece_idx][sq.0 as usize];
        }
    }

    #[inline]
    pub fn legal_moves(&self) -> MoveList {
        let (pinned_bb, checkers) = self.find_pinned();
        let own_combined = match self.side_to_move {
            Color::White => self.white,
            Color::Black => self.black,
        };
        let mut moves = MoveList::new();

        let ksq = self.ksq;

        let check_mask = if checkers.0.count_ones() == 1 {
            between(checkers.lsb_square(), ksq) ^ checkers
        } else {
            Bitboard::universe()
        };

        if checkers.0.count_ones() <= 1 {
            Pawn::legal_moves(
                self,
                checkers,
                pinned_bb,
                self.pieces(self.side_to_move)[PieceType::Pawn as usize],
                own_combined,
                &mut moves,
                check_mask,
            );
            Knight::legal_moves(
                self,
                checkers,
                pinned_bb,
                self.pieces(self.side_to_move)[PieceType::Knight as usize],
                own_combined,
                &mut moves,
                check_mask,
            );

            Rook::legal_moves(
                self,
                checkers,
                pinned_bb,
                self.pieces(self.side_to_move)[PieceType::Rook as usize],
                own_combined,
                &mut moves,
                check_mask,
            );

            Bishop::legal_moves(
                self,
                checkers,
                pinned_bb,
                self.pieces(self.side_to_move)[PieceType::Bishop as usize],
                own_combined,
                &mut moves,
                check_mask,
            );

            Queen::legal_moves(
                self,
                checkers,
                pinned_bb,
                self.pieces(self.side_to_move)[PieceType::Queen as usize],
                own_combined,
                &mut moves,
                check_mask,
            );
            King::legal_moves(
                self,
                checkers,
                pinned_bb,
                Bitboard::from_square(ksq),
                own_combined,
                &mut moves,
                check_mask,
            );
        } else {
            King::legal_moves(
                self,
                checkers,
                pinned_bb,
                Bitboard::from_square(ksq),
                own_combined,
                &mut moves,
                check_mask,
            );
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

    pub fn make_move_new(&self, m: &Move) -> Board {
        let mut board = self.clone();
        unsafe { board.make_move_unchecked(m) }
        board
    }

    /// Updates all the bitboards, which are involved in move, updates side to move and moves list.
    ///
    /// # Safety
    /// This method does not check if provided move is a valid move, it may break representation of
    /// the game. Use it only with moves, received from [`Self::pseudo_legal_moves`]. Otherwise use [`Self::make_move`].
    #[inline]
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

        if piece == PieceType::Pawn || captured.is_some() {
            self.halfmoves = 0;
        }
        if captured.is_none() && piece != PieceType::Pawn {
            self.halfmoves += 1;

            if self.halfmoves == 100 {
                self.status = Status::Draw(DrawReason::Halfmoves);
            }
        }
        // update castling rights
        CastlingRights::update_castling_rights(self, piece, captured, m);

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
        self.update_position(m, piece, captured);
        self.side_to_move = self.side_to_move.opposite();
        self.ksq = self.pieces(self.side_to_move)[PieceType::King as usize].lsb_square();
        self.hash ^= hashing::SIDE_KEY;
    }

    #[inline]
    fn update_position(&mut self, m: &Move, piece: PieceType, capture: Option<PieceType>) {
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
            if checkers.0.count_ones() == 1 {
                let attacker_pawn = match self.side_to_move {
                    Color::White => Bitboard::from_square(target_square) >> 8,
                    Color::Black => Bitboard::from_square(target_square) << 8,
                };
                if attacker_pawn & check_mask == 0
                    && Bitboard::from_square(target_square) & check_mask == 0
                {
                    return;
                }
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
                let king_square = self.ksq;
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
        let king = self.ksq;
        let enemy_pieces = self.pieces(self.side_to_move.opposite());
        let enemy_combined = self.pieces_combined(self.side_to_move.opposite());
        let bishop_rays = Bitboard(
            BISHOP_ATTACKS[king.0 as usize]
                .into_iter()
                .reduce(|acc, next| acc | next)
                .unwrap()
                .to_owned(),
        ) & (enemy_pieces[PieceType::Bishop as usize]
            | enemy_pieces[PieceType::Queen as usize]);

        let rook_rays = Bitboard(
            ROOK_ATTACKS[king.0 as usize]
                .into_iter()
                .reduce(|acc, next| acc | next)
                .unwrap()
                .to_owned(),
        ) & (enemy_pieces[PieceType::Rook as usize]
            | enemy_pieces[PieceType::Queen as usize]);

        let attackers = enemy_combined & (bishop_rays | rook_rays);
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
                & enemy_combined
                & enemy_pieces[PieceType::Knight as usize];

        let pawn_attacks =
            Pawn::pawn_attacks(self.side_to_move, king) & enemy_pieces[PieceType::Pawn as usize];
        checkers ^= knight_moves;
        checkers ^= pawn_attacks;

        (pinned, checkers)
    }

    #[inline]
    pub fn get_ray(&self, sq1: Square, sq2: Square) -> Bitboard {
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
        let castling_rights = self.castling_rights[self.side_to_move as usize];
        let king_on_original_square = castling_rights != CastlingRights::NoCastling;
        if !king_on_original_square {
            return None;
        }
        let a_rook_on_original_square =
            castling_rights == CastlingRights::Both || castling_rights == CastlingRights::QueenSide;
        let h_rook_on_original_square =
            castling_rights == CastlingRights::Both || castling_rights == CastlingRights::KingSide;

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
        let mut board = Self {
            white_pieces,
            black_pieces,
            white,
            black,
            halfmoves: 0,
            side_to_move: Color::White,
            status: Status::Ongoing,
            castling_rights: [CastlingRights::Both, CastlingRights::Both],
            en_passant_square: None,
            ksq: Square::E1,
            hash: 0,
            tt: Arc::new(Mutex::new(TranspositionTable::with_capacity(4096))),
        };
        board.init_hash();
        board
    }
}

impl From<FEN> for Board {
    fn from(fen: FEN) -> Self {
        let white = Self::combine(fen.pieces[Color::White as usize]);
        let black = Self::combine(fen.pieces[Color::Black as usize]);
        let mut board = Self {
            white_pieces: fen.pieces[Color::White as usize],
            black_pieces: fen.pieces[Color::Black as usize],
            white,
            black,
            halfmoves: fen.halfmove_clock,
            side_to_move: fen.active_color,
            status: if fen.halfmove_clock < 100 {
                Status::Ongoing
            } else {
                Status::Draw(DrawReason::Halfmoves)
            },
            castling_rights: fen.castling_rules,
            en_passant_square: fen.en_passant_target,
            ksq: fen.pieces[fen.active_color as usize][PieceType::King as usize].lsb_square(),
            hash: 0,
            tt: Arc::new(Mutex::new(TranspositionTable::with_capacity(4096))),
        };
        board.init_hash();
        board
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
        if self.board.status != Status::Ongoing {
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
    use crate::perft::perft;
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
        assert!(board.en_passant_square.is_some());
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
        let board = Board::default();
        assert_eq!(perft(&board, 1), 20);
    }

    #[test]
    fn perft_starting_position_depth_2() {
        let board = Board::default();
        assert_eq!(perft(&board, 2), 400);
    }

    #[test]
    fn perft_starting_position_depth_3() {
        let board = Board::default();
        assert_eq!(perft(&board, 3), 8902);
    }

    #[test]
    fn perft_starting_position_depth_4() {
        let board = Board::default();
        assert_eq!(perft(&board, 4), 197_281);
    }

    #[test]
    fn perft_starting_position_depth_5() {
        let board = Board::default();
        assert_eq!(perft(&board, 5), 4_865_609);
    }

    #[test]
    fn perft_depth_6() {
        let board = Board::from_fen("5K2/8/1Q6/2N5/8/1p2k3/8/8 w - - 0 1").unwrap();
        assert_eq!(perft(&board, 5), 1004658);
    }

    #[test]
    fn perft_kiwipete_depth_1() {
        let board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&board, 1), 48);
    }

    #[test]
    fn perft_kiwipete_depth_2() {
        let board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&board, 2), 2039);
    }

    #[test]
    fn perft_kiwipete_depth_3() {
        let board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&board, 3), 97862);
    }

    #[test]
    fn perft_kiwipete_depth_4() {
        let board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(perft(&board, 4), 4085603);
    }

    #[test]
    fn position_3_depth_1() {
        let board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
        assert_eq!(perft(&board, 1), 14);
    }

    #[test]
    fn position_3_depth_2() {
        let board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
        assert_eq!(perft(&board, 2), 191);
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
