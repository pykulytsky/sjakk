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
    pub key: u64,
    pub depth: usize,
    pub eval: f32,
    pub best_move: Option<Move>,
    pub node_type: NodeType,
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
