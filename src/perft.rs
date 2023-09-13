use crate::Move;
use std::sync::atomic::Ordering;
use std::{collections::BTreeMap, sync::Mutex};
use std::{
    collections::VecDeque,
    sync::{atomic::AtomicU32, Arc},
};

use futures::executor::ThreadPool;

use crate::board::Board;

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
pub fn perft_divide(board: &mut Board, depth: usize) -> (BTreeMap<Move, usize>, usize) {
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

#[inline]
pub fn perft_async_divide(
    board: &Board,
    depth: usize,
    executor: &ThreadPool,
) -> (BTreeMap<Move, usize>, usize) {
    let moves = board.legal_moves();

    let final_depth = Arc::new(AtomicU32::new(0));
    let mut results = VecDeque::new();
    let barrier = Arc::new(AtomicU32::new(moves.len() as u32));
    let nodes = Arc::new(Mutex::new(BTreeMap::new()));

    for m in moves.iter() {
        let m = *m;
        let final_depth = final_depth.clone();
        let mut board = board.clone();
        let barrier = barrier.clone();
        let node_list = nodes.clone();
        results.push_back(async move {
            let result = async move {
                let mut board = board.make_move_new(&m);
                let nodes = perft(&mut board, depth - 1);
                final_depth.fetch_add(nodes as u32, Ordering::Release);
                node_list.lock().unwrap().insert(m, nodes);
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
    let nodes = Arc::into_inner(nodes).unwrap().into_inner().unwrap();
    (nodes, final_depth.load(Ordering::Acquire) as usize)
}
