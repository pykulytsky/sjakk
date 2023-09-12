use crate::bitboard::Bitboard;
use crate::square::Square;
use std::fs::File;
use std::io::Write;

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

const fn _between(a: i8, t: i8, b: i8) -> bool {
    if a < b {
        a < t && t < b
    } else {
        b < t && t < a
    }
}

pub static mut BETWEEN: [[Bitboard; 64]; 64] = [[Bitboard(0); 64]; 64];

pub fn gen_between() {
    for src in ALL_SQUARES.iter() {
        for dest in ALL_SQUARES.iter() {
            unsafe {
                BETWEEN[src.0 as usize][dest.0 as usize] = ALL_SQUARES
                    .iter()
                    .filter(|test| {
                        let src_rank = src.rank() as i8;
                        let src_file = src.file() as i8;
                        let dest_rank = dest.rank() as i8;
                        let dest_file = dest.file() as i8;
                        let test_rank = test.rank() as i8;
                        let test_file = test.file() as i8;

                        // test diagonals first, as above
                        if (src_rank - dest_rank).abs() == (src_file - dest_file).abs()
                            && *src != *dest
                        {
                            (src_rank - test_rank).abs() == (src_file - test_file).abs()
                                && (dest_rank - test_rank).abs() == (dest_file - test_file).abs()
                                && _between(src_rank, test_rank, dest_rank)
                        } else if (src_rank == dest_rank || src_file == dest_file) && *src != *dest
                        {
                            (src_rank == test_rank
                                && dest_rank == test_rank
                                && _between(src_file, test_file, dest_file))
                                || (src_file == test_file
                                    && dest_file == test_file
                                    && _between(src_rank, test_rank, dest_rank))
                        } else {
                            false
                        }
                    })
                    .fold(Bitboard(0), |b, s| b | Bitboard::from_square(*s));
            }
        }
    }
}

#[allow(clippy::needless_range_loop)]
pub fn write_between(f: &mut File) {
    gen_between();
    writeln!(f, "const BETWEEN: [[BitBoard; 64]; 64] = [[").unwrap();
    for i in 0..64 {
        for j in 0..64 {
            unsafe { writeln!(f, "    BitBoard({}),", BETWEEN[i][j].0).unwrap() };
        }
        if i != 63 {
            writeln!(f, "  ], [").unwrap();
        }
    }
    writeln!(f, "]];").unwrap();
}
