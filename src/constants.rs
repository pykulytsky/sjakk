//! Provides some useful constants, that are used throughout the crate.
use strum_macros::EnumIter;
/// Represents all possible directions in chess, that piece can move in.
///
///  northwest    north   northeast
///  noWe         nort         noEa
///          
///              \  |  /
///  west       <-  0 ->       east
///              /  |  \
///
///  soWe         sout         soEa
///  southwest    south   southeast
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

pub fn test(dir: Direction) {
    let arr = [1, 2, 3, 4, 5, 6, 7, 8];
    println!("{}", arr[dir as usize]);
}
