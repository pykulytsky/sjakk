use futures::executor::ThreadPool;

use crate::{
    board::Board,
    evaluation::evaluate_relative,
    transposition_table::{NodeType, TTEntry},
    Move,
};
use std::{
    collections::{hash_map::Entry, VecDeque},
    sync::atomic::{AtomicI32, AtomicU32, Ordering},
    time::Instant,
};
use std::{sync::Mutex, time::Duration};

use crate::transposition_table::TranspositionTable;

use std::sync::Arc;

pub mod mvv_lva;

pub fn quiesce(
    board: &Board,
    mut alpha: f32,
    beta: f32,
    tt: Arc<Mutex<TranspositionTable>>,
) -> f32 {
    let mut hash_move = None;
    if let Entry::Occupied(ttentry) = tt.lock().unwrap().entry(board.hash) {
        let ttentry = ttentry.get();
        if let Some(mv) = ttentry.best_move {
            hash_move = Some(mv);
        }
        match ttentry.node_type {
            NodeType::PV => return ttentry.eval,
            NodeType::All if ttentry.eval <= alpha => return ttentry.eval,
            NodeType::Cut if ttentry.eval >= beta => return ttentry.eval,
            _ => {}
        }
    }

    let moves = board.legal_moves();
    let mut score: f32 = f32::MIN;
    let (_, checkers) = board.find_pinned();
    if moves.is_empty() {
        if checkers.popcnt() == 0 {
            return 0.0;
        } else {
            return score;
        }
    }
    let stand_pat = evaluate_relative(board);
    if stand_pat >= beta {
        tt.lock().unwrap().insert(
            board.hash,
            TTEntry::new(board.hash, 0, score, None, NodeType::Cut),
        );
        return beta;
    }
    if alpha < stand_pat {
        alpha = stand_pat;
    }
    let moves = mvv_lva::score_moves(moves, board, hash_move);
    let captures = moves.iter().filter(|m| board.captured_piece(m).is_some());
    let mut node_type = NodeType::Cut;
    for capture in captures {
        let board = board.make_move_new(capture);
        score = -quiesce(&board, -beta, -alpha, tt.clone());
        if score >= beta {
            tt.lock().unwrap().insert(
                board.hash,
                TTEntry::new(
                    board.hash,
                    0,
                    score,
                    Some(capture.to_owned()),
                    NodeType::Cut,
                ),
            );
            return beta;
        }
        if score > alpha {
            node_type = NodeType::PV;
            alpha = score;
        } else if score <= alpha {
            node_type = NodeType::All;
        }
    }

    tt.lock().unwrap().insert(
        board.hash,
        TTEntry::new(board.hash, 0, alpha, None, node_type),
    );
    alpha
}

#[inline]
pub fn negamax(board: &Board, depth: usize) -> f32 {
    if depth == 0 {
        return evaluate_relative(board);
    }
    let mut max = -f32::MAX;
    let moves = board.legal_moves();
    for m in moves.iter() {
        let board = board.make_move_new(m);
        let score = -negamax(&board, depth - 1);
        if score > max {
            max = score;
        }
    }
    max
}

#[inline]
pub fn alpha_beta_negamax(
    board: &Board,
    mut alpha: f32,
    beta: f32,
    depth: usize,
    tt: Arc<Mutex<TranspositionTable>>,
    time_limit: Arc<Instant>,
) -> f32 {
    if depth == 0 || Instant::now() > *time_limit {
        return quiesce(board, alpha, beta, tt.clone());
    }
    let mut hash_move = None;
    if let Entry::Occupied(ttentry) = tt.lock().unwrap().entry(board.hash) {
        let ttentry = ttentry.get();
        if let Some(mv) = ttentry.best_move {
            hash_move = Some(mv);
        }
        if ttentry.depth >= depth {
            match ttentry.node_type {
                NodeType::PV => return ttentry.eval,
                NodeType::All if ttentry.eval <= alpha => return ttentry.eval,
                NodeType::Cut if ttentry.eval >= beta => return ttentry.eval,
                _ => {}
            }
        }
    }

    let mut node_type = NodeType::Cut;

    let moves = board.legal_moves();
    let moves = mvv_lva::score_moves(moves, board, hash_move);
    for m in moves.iter() {
        let board = board.make_move_new(m);
        let score = alpha_beta_negamax(
            &board,
            -beta,
            -alpha,
            depth - 1,
            tt.clone(),
            time_limit.clone(),
        );

        if score >= beta {
            // cutoff
            tt.lock().unwrap().insert(
                board.hash,
                TTEntry::new(board.hash, depth, score, Some(m.to_owned()), NodeType::Cut),
            );
            return score;
        }
        if score > alpha {
            // exact
            alpha = score;
            hash_move = Some(m.to_owned());
            node_type = NodeType::PV;
        } else if score <= alpha {
            node_type = NodeType::All;
        }
    }
    tt.lock().unwrap().insert(
        board.hash,
        TTEntry::new(board.hash, depth, alpha, hash_move, node_type),
    );
    alpha
}

#[inline]
pub fn alpha_beta_negamax_root_async(
    board: &Board,
    depth: usize,
    executor: &ThreadPool,
    time_limit: Duration,
) -> (f32, Option<Move>) {
    let time = Arc::new(Instant::now() + time_limit);
    if Instant::now() > *time {
        return (0.0, None);
    }
    let moves = board.legal_moves();
    if moves.is_empty() {
        return (f32::MIN, None);
    }
    let mut results = VecDeque::new();
    let move_number = Arc::new(AtomicU32::new(0));
    let barrier = Arc::new(AtomicU32::new(moves.len() as u32));
    let alpha = Arc::new(AtomicI32::new(i32::MIN));
    let beta = Arc::new(f32::MAX);

    let moves = mvv_lva::score_moves(
        moves,
        board,
        board
            .tt
            .lock()
            .unwrap()
            .get(&board.hash)
            .and_then(|entry| entry.best_move),
    );
    for (i, m) in moves.iter().enumerate() {
        let m = m.clone().to_owned();
        let barrier = barrier.clone();
        let move_number = move_number.clone();
        let tt = board.tt.clone();
        let b = board.clone();
        let alpha = alpha.clone();
        let beta = beta.clone();
        let time = time.clone();
        results.push_back(async move {
            let result = async move {
                let board = b.make_move_new(&m);
                let score = -alpha_beta_negamax(
                    &board,
                    -(*beta),
                    -(alpha.load(Ordering::Acquire) as f32 / 1000.0),
                    depth - 1,
                    tt.clone(),
                    time,
                );
                if score >= *beta {
                    alpha.store((*beta * 1000.0) as i32, Ordering::Release);
                    move_number.store(i as u32, Ordering::Release);
                    return;
                }
                if score > alpha.load(Ordering::Acquire) as f32 / 1000.0 {
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
pub fn negamax_root(board: &Board, depth: usize) -> (f32, Option<Move>) {
    let mut max = -f32::MAX;
    let moves = board.legal_moves();
    if moves.is_empty() {
        return (max, None);
    }
    let mut mv = moves.inner.get(0).copied();
    for m in moves.iter() {
        let board = board.make_move_new(m);
        let score = -negamax(&board, depth - 1);
        if score > max {
            max = score;
            mv = Some(m.to_owned());
        }
    }
    (max, mv)
}

#[inline]
pub fn negamax_root_async(
    board: &Board,
    depth: usize,
    executor: &ThreadPool,
) -> (i32, Option<Move>) {
    let moves = board.legal_moves();
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
        let b = board.clone();
        results.push_back(async move {
            let result = async move {
                let board = b.make_move_new(&m);
                let score = -negamax(&board, depth - 1);
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
