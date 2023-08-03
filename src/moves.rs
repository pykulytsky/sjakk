use crate::Square;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Move {
    from: Square,
    to: Square,
    capture: bool,
    promotion: bool,
}
