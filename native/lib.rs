use hextree::Cell;
use hextree::HexTreeSet;
use rustler::types::atom::{false_, ok, true_};
use rustler::{atoms, Atom, Encoder, Env, ResourceArc, Term};
use std::fs::File;
use std::sync::Mutex;

#[rustler::nif]
fn hexset_new<'a>(env: Env<'a>, indices: Vec<u64>) -> Term<'a> {
    let cells: Vec<Cell> = indices
        .iter()
        .map(|&idx| Cell::try_from(idx).unwrap())
        .collect();
    let set: HexTreeSet = cells.iter().collect();
    (atoms::ok(), ResourceArc::new(HexSet { set })).encode(env)
}

#[rustler::nif]
fn hexset_contains(set: ResourceArc<HexSet>, h3: u64) -> Atom {
    let idx = Cell::try_from(h3).unwrap();
    if set.set.contains(idx) {
        true_()
    } else {
        false_()
    }
}

#[rustler::nif]
fn hexset_to_disktree(set: ResourceArc<HexSet>, filename: String) -> Atom {
    let mut output = File::create(filename).unwrap();
    set.set
        .to_disktree(&mut output, |wtr, val| bincode::serialize_into(wtr, val))
        .unwrap();
    ok()
}

#[rustler::nif]
fn disktree_open<'a>(env: Env<'a>, filename: String) -> Term<'a> {
    let input = File::open(filename).unwrap();
    let dt = hextree::disktree::DiskTree::from_reader(input).unwrap();
    (atoms::ok(), ResourceArc::new(DiskTree { dt: dt.into() })).encode(env)
}

#[rustler::nif]
fn disktree_contains(dt: ResourceArc<DiskTree>, h3: u64) -> Atom {
    let idx = Cell::try_from(h3).unwrap();
    if dt.dt.lock().unwrap().contains(idx).unwrap() {
        true_()
    } else {
        false_()
    }
}

struct HexSet {
    set: HexTreeSet,
}

struct DiskTree {
    dt: Mutex<hextree::disktree::DiskTree<File>>,
}

mod atoms {
    super::atoms! {
        ok,
    }
}

pub fn load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(HexSet, env);
    rustler::resource!(DiskTree, env);
    true
}

rustler::init!(
    "hextree_nif",
    [
        hexset_new,
        hexset_contains,
        hexset_to_disktree,
        disktree_open,
        disktree_contains,
    ],
    load = load
);
