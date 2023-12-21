use rustler::{
    atoms, Atom, Binary, Encoder, Env, ListIterator, NewBinary, OwnedBinary, ResourceArc, Term,
};

use rustler::types::atom::{ok, true_, false_};
use std::{
    borrow::Borrow,
    hash::{Hash, Hasher},
    sync::Arc,
    time::Duration,
};

use hextree::HexTreeSet;
use hextree::Cell;

#[rustler::nif]
fn hexset_new<'a>(env: Env<'a>, indices: Vec<u64>) -> Term<'a> {
    let cells: Vec<Cell> = indices
        .iter()
        .map(|&idx| Cell::try_from(idx).unwrap())
        .collect();
    let set: HexTreeSet = cells.iter().collect();
    (
        atoms::ok(),
        ResourceArc::new(HexSet{ set })
        )
        .encode(env)
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


struct HexSet {
    set: HexTreeSet
}

mod atoms {
    super::atoms! {
        ok,
    }
}

pub fn load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(HexSet, env);
    true
}

rustler::init!(
    "hextree_nif",
    [
    hexset_new,
    hexset_contains,
    ],
    load = load
    );

