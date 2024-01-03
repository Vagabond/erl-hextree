use hextree::{Cell, HexTreeSet};
use rustler::{
    types::atom::{false_, ok, true_},
    Atom, Encoder, Env, ListIterator, ResourceArc, Term,
};
use std::{fs::File, sync::Mutex};

#[rustler::nif]
fn hexset_new<'a>(env: Env<'a>, indices: ListIterator<'a>) -> Term<'a> {
    let set: HexTreeSet = indices
        .map(|term| {
            let h3_index: u64 = term.decode().unwrap();
            Cell::try_from(h3_index).unwrap()
        })
        .collect();
    (ok(), ResourceArc::new(HexSet(set))).encode(env)
}

#[rustler::nif]
fn hexset_contains(set: ResourceArc<HexSet>, h3: u64) -> Atom {
    let idx = Cell::try_from(h3).unwrap();
    if set.contains(idx) {
        true_()
    } else {
        false_()
    }
}

#[rustler::nif]
fn hexset_to_disktree(set: ResourceArc<HexSet>, filename: String) -> Atom {
    let mut output = File::create(filename).unwrap();
    set.to_disktree(&mut output, |wtr, val| bincode::serialize_into(wtr, val))
        .unwrap();
    ok()
}

#[rustler::nif]
fn disktree_open<'a>(env: Env<'a>, filename: String) -> Term<'a> {
    let input = File::open(filename).unwrap();
    let dt = hextree::disktree::DiskTree::from_reader(input).unwrap();
    (ok(), ResourceArc::new(DiskTree(dt.into()))).encode(env)
}

#[rustler::nif]
fn disktree_contains(dt: ResourceArc<DiskTree>, h3: u64) -> Atom {
    let idx = Cell::try_from(h3).unwrap();
    if dt.lock().unwrap().contains(idx).unwrap() {
        true_()
    } else {
        false_()
    }
}

struct HexSet(HexTreeSet);

impl std::ops::Deref for HexSet {
    type Target = HexTreeSet;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

struct DiskTree(Mutex<hextree::disktree::DiskTree<File>>);

impl std::ops::Deref for DiskTree {
    type Target = Mutex<hextree::disktree::DiskTree<File>>;
    fn deref(&self) -> &Self::Target {
        &self.0
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
