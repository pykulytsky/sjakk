//! Provides some useful constants, that are used throughout the crate.
use crate::Square;
use strum_macros::EnumIter;

/// Represents all possible directions in chess, in which piece can move.
///
///  northwest    north   northeast
///  noWe         nort         noEa
///  `       
///              \  |  /
///  west       <-  0 ->       east
///              /  |  \
///
///  soWe         sout         soEa
///  southwest    south   southeast
///  `
#[derive(Debug, EnumIter)]
pub enum Direction {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
}

pub const CLEAR_RANK: [u64; 8] = [
    u64::MAX << 8,
    u64::MAX << 16 | 0xFF_u64,
    (u64::MAX << 16 | 0xFF_u64) << 8 | 0xFF_u64,
    (u64::MAX << 16 | 0xFF_u64) << 16 | 0xFFFF_u64,
    (u64::MAX >> 16 | 0xFF_u64.reverse_bits()) >> 16 | 0xFFFF_u64.reverse_bits(),
    (u64::MAX >> 16 | 0xFF_u64.reverse_bits()) >> 8 | 0xFF_u64.reverse_bits(),
    u64::MAX >> 16 | 0xFF_u64.reverse_bits(),
    u64::MAX >> 8,
];

pub const MASK_RANK: [u64; 8] = [
    !CLEAR_RANK[0],
    !CLEAR_RANK[1],
    !CLEAR_RANK[2],
    !CLEAR_RANK[3],
    !CLEAR_RANK[4],
    !CLEAR_RANK[5],
    !CLEAR_RANK[6],
    !CLEAR_RANK[7],
];

const FILE: u64 = u64::from_ne_bytes([
    0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001,
]);

pub const MASK_FILE: [u64; 8] = [
    FILE,
    FILE << 1_u8,
    FILE << 2_u8,
    FILE << 3_u8,
    FILE << 4_u8,
    FILE << 5_u8,
    FILE << 6_u8,
    FILE << 7_u8,
];

pub const CLEAR_FILE: [u64; 8] = [
    !MASK_FILE[0],
    !MASK_FILE[1],
    !MASK_FILE[2],
    !MASK_FILE[3],
    !MASK_FILE[4],
    !MASK_FILE[5],
    !MASK_FILE[6],
    !MASK_FILE[7],
];

pub const INDEX64: [u64; 64] = [
    0, 1, 48, 2, 57, 49, 28, 3, 61, 58, 50, 42, 38, 29, 17, 4, 62, 55, 59, 36, 53, 51, 43, 22, 45,
    39, 33, 30, 24, 18, 12, 5, 63, 47, 56, 27, 60, 41, 37, 16, 54, 35, 52, 21, 44, 32, 23, 11, 46,
    26, 40, 15, 34, 20, 31, 10, 25, 14, 19, 9, 13, 8, 7, 6,
];

pub const LSB_64_TABLE: [u64; 64] = [
    63, 30, 3, 32, 59, 14, 11, 33, 60, 24, 50, 9, 55, 19, 21, 34, 61, 29, 2, 53, 51, 23, 41, 18,
    56, 28, 1, 43, 46, 27, 0, 35, 62, 31, 58, 4, 5, 49, 54, 6, 15, 52, 12, 40, 7, 42, 45, 16, 25,
    57, 48, 13, 10, 39, 8, 44, 20, 47, 38, 22, 17, 37, 36, 26,
];

pub const DEBRUIJ_T: &[u8] = &[
    0, 47, 1, 56, 48, 27, 2, 60, 57, 49, 41, 37, 28, 16, 3, 61, 54, 58, 35, 52, 50, 42, 21, 44, 38,
    32, 29, 23, 17, 11, 4, 62, 46, 55, 26, 59, 40, 36, 15, 53, 34, 51, 20, 43, 31, 22, 10, 45, 25,
    39, 14, 33, 19, 30, 9, 24, 13, 18, 8, 12, 7, 6, 5, 63,
];

pub const DEBRUIJ_M: u64 = 0x03f7_9d71_b4cb_0a89;

pub const WHITE_PAWNS: u64 = 0b0000000000000000000000000000000000000000000000001111111100000000_u64;
pub const BLACK_PAWNS: u64 = WHITE_PAWNS << 40;
pub const WHITE_AND_BLACK_PAWNS: u64 = WHITE_PAWNS | BLACK_PAWNS;

pub const WHITE_ROOKS: u64 = 0b0000000000000000000000000000000000000000000000000000000010000001_u64;
pub const BLACK_ROOKS: u64 = WHITE_ROOKS << 56;

pub const WHITE_KNIGHTS: u64 =
    0b0000000000000000000000000000000000000000000000000000000001000010_u64;
pub const BLACK_KNIGHTS: u64 = WHITE_KNIGHTS << 56;

pub const WHITE_BISHOPS: u64 =
    0b0000000000000000000000000000000000000000000000000000000000100100_u64;
pub const BLACK_BISHOPS: u64 = WHITE_BISHOPS << 56;

pub const WHITE_QUEENS: u64 =
    0b0000000000000000000000000000000000000000000000000000000000001000_u64;
pub const BLACK_QUEENS: u64 = WHITE_QUEENS << 56;

pub const WHITE_KING: u64 = 0b0000000000000000000000000000000000000000000000000000000000010000_u64;
pub const BLACK_KING: u64 = WHITE_KING << 56;

pub const WHITE_PIECES: u64 =
    WHITE_PAWNS | WHITE_ROOKS | WHITE_KNIGHTS | WHITE_BISHOPS | WHITE_QUEENS | WHITE_KING;
pub const BLACK_PIECES: u64 =
    BLACK_PAWNS | BLACK_ROOKS | BLACK_KNIGHTS | BLACK_BISHOPS | BLACK_QUEENS | BLACK_KING;

pub const ALL_PIECES: u64 = WHITE_PIECES | BLACK_PIECES;

pub const WHITE_SQUARES: u64 = 0x55AA55AA55AA55AA;
pub const BLACK_SQUARES: u64 = 0xAA55AA55AA55AA55;

pub const WHITE_LONG_CASTLE_MASK: u64 = 0b10001;
pub const WHITE_SHORT_CASTLE_MASK: u64 = 0b10001 << 4;
pub const BLACK_SHORT_CASTLE_MASK: u64 = WHITE_SHORT_CASTLE_MASK.reverse_bits();
pub const BLACK_LONG_CASTLE_MASK: u64 = WHITE_LONG_CASTLE_MASK.reverse_bits();

pub const ALL_SQUARES: [Square; 64] = [
    Square(0),
    Square(1),
    Square(2),
    Square(3),
    Square(4),
    Square(5),
    Square(6),
    Square(7),
    Square(8),
    Square(9),
    Square(10),
    Square(11),
    Square(12),
    Square(13),
    Square(14),
    Square(15),
    Square(16),
    Square(17),
    Square(18),
    Square(19),
    Square(20),
    Square(21),
    Square(22),
    Square(23),
    Square(24),
    Square(25),
    Square(26),
    Square(27),
    Square(28),
    Square(29),
    Square(30),
    Square(31),
    Square(32),
    Square(33),
    Square(34),
    Square(35),
    Square(36),
    Square(37),
    Square(38),
    Square(39),
    Square(40),
    Square(41),
    Square(42),
    Square(43),
    Square(44),
    Square(45),
    Square(46),
    Square(47),
    Square(48),
    Square(49),
    Square(50),
    Square(51),
    Square(52),
    Square(53),
    Square(54),
    Square(55),
    Square(56),
    Square(57),
    Square(58),
    Square(59),
    Square(60),
    Square(61),
    Square(62),
    Square(63),
];
