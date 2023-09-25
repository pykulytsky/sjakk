use std::collections::HashMap;

use crate::Move;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum NodeType {
    PV,
    All,
    Cut,
}

#[derive(Debug, Clone, Copy)]
pub struct TTEntry {
    pub(crate) key: u64,
    pub(crate) depth: usize,
    pub(crate) eval: f32,
    pub(crate) best_move: Option<Move>,
    pub(crate) node_type: NodeType,
}

impl TTEntry {
    pub fn new(
        key: u64,
        depth: usize,
        eval: f32,
        best_move: Option<Move>,
        node_type: NodeType,
    ) -> Self {
        Self {
            key,
            depth,
            eval,
            best_move,
            node_type,
        }
    }
}

pub type TranspositionTable = HashMap<u64, TTEntry>;
