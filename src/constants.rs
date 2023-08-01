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
