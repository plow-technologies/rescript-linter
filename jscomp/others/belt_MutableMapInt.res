module I = Belt_internalMapInt
type key = int

module N = Belt_internalAVLtree
module A = Belt_Array

type t<'a> = {mutable data: I.t<'a>}

let make = () => {data: None}
let isEmpty = m => N.isEmpty(m.data)
let clear = m => m.data = None
/* let singleton k v = t ~data:(N.singleton k v) */

let minKeyUndefined = m => N.minKeyUndefined(m.data)
let minKey = m => N.minKey(m.data)
let maxKeyUndefined = m => N.maxKeyUndefined(m.data)
let maxKey = m => N.maxKey(m.data)
let minimum = m => N.minimum(m.data)
let minUndefined = m => N.minUndefined(m.data)
let maximum = m => N.maximum(m.data)
let maxUndefined = m => N.maxUndefined(m.data)

let set = (m: t<_>, k, v) => {
  let old_data = m.data
  let v = I.addMutate(old_data, k, v)
  if v !== old_data {
    m.data = v
  }
}

let forEachU = (d, f) => N.forEachU(d.data, f)
let forEach = (d, f) => forEachU(d, (. a, b) => f(a, b))
let mapU = (d, f) => {data: N.mapU(d.data, f)}
let map = (d, f) => mapU(d, (. a) => f(a))
let mapWithKeyU = (d, f) => {data: N.mapWithKeyU(d.data, f)}
let mapWithKey = (d, f) => mapWithKeyU(d, (. a, b) => f(a, b))
let reduceU = (d, acc, f) => N.reduceU(d.data, acc, f)
let reduce = (d, acc, f) => reduceU(d, acc, (. a, b, c) => f(a, b, c))
let everyU = (d, f) => N.everyU(d.data, f)
let every = (d, f) => everyU(d, (. a, b) => f(a, b))
let someU = (d, f) => N.someU(d.data, f)
let some = (d, f) => someU(d, (. a, b) => f(a, b))
let size = d => N.size(d.data)
let toList = d => N.toList(d.data)
let toArray = d => N.toArray(d.data)
let keysToArray = d => N.keysToArray(d.data)
let valuesToArray = d => N.valuesToArray(d.data)
let checkInvariantInternal = d => N.checkInvariantInternal(d.data)
let has = (d, v) => I.has(d.data, v)

let rec removeMutateAux = (nt, x: key) => {
  let k = nt.N.key
  if x == k {
    let {N.left: l, right: r} = nt
    switch (l, r) {
    | (None, _) => r
    | (_, None) => l
    | (_, Some(nr)) =>
      nt.right = N.removeMinAuxWithRootMutate(nt, nr)
      Some(N.balMutate(nt))
    }
  } else if x < k {
    switch nt.left {
    | None => Some(nt)
    | Some(l) =>
      nt.left = removeMutateAux(l, x)
      Some(N.balMutate(nt))
    }
  } else {
    switch nt.right {
    | None => Some(nt)
    | Some(r) =>
      nt.right = removeMutateAux(r, x)
      Some(N.balMutate(nt))
    }
  }
}

let remove = (d, v) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => ()
  | Some(root) =>
    let newRoot = removeMutateAux(root, v)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
  }
}

let rec updateDone = (t, x: key, f) =>
  switch t {
  | None =>
    switch f(. None) {
    | Some(data) => N.singleton(x, data)
    | None => t
    }
  | Some(nt) =>
    let k = nt.N.key

    /* let  c = (Belt_Cmp.getCmpInternal cmp) x k [@bs] in */
    if k == x {
      switch f(. Some(nt.value)) {
      | None =>
        let {N.left: l, right: r} = nt
        switch (l, r) {
        | (None, _) => r
        | (_, None) => l
        | (_, Some(nr)) =>
          nt.right = N.removeMinAuxWithRootMutate(nt, nr)
          Some(N.balMutate(nt))
        }
      | Some(data) =>
        nt.value = data
        Some(nt)
      }
    } else {
      let {N.left: l, right: r} = nt
      if x < k {
        let ll = updateDone(l, x, f)
        nt.left = ll
      } else {
        nt.right = updateDone(r, x, f)
      }
      Some(N.balMutate(nt))
    }
  }

let updateU = (t, x, f) => {
  let oldRoot = t.data
  let newRoot = updateDone(oldRoot, x, f)
  if newRoot !== oldRoot {
    t.data = newRoot
  }
}
let update = (t, x, f) => updateU(t, x, (. a) => f(a))
let rec removeArrayMutateAux = (t, xs, i, len) =>
  if i < len {
    let ele = A.getUnsafe(xs, i)
    let u = removeMutateAux(t, ele)
    switch u {
    | None => None
    | Some(t) => removeArrayMutateAux(t, xs, i + 1, len)
    }
  } else {
    Some(t)
  }

let removeMany = (d: t<_>, xs) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => ()
  | Some(nt) =>
    let len = A.length(xs)
    let newRoot = removeArrayMutateAux(nt, xs, 0, len)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
  }
}

/* let split = I.split */
/* let merge = I.merge */

let fromArray = xs => {data: I.fromArray(xs)}

let cmpU = (d0, d1, f) => I.cmpU(d0.data, d1.data, f)
let cmp = (d0, d1, f) => cmpU(d0, d1, (. a, b) => f(a, b))

let eqU = (d0, d1, f) => I.eqU(d0.data, d1.data, f)
let eq = (d0, d1, f) => eqU(d0, d1, (. a, b) => f(a, b))

let get = (d, x) => I.get(d.data, x)
let getUndefined = (d, x) => I.getUndefined(d.data, x)
let getWithDefault = (d, x, def) => I.getWithDefault(d.data, x, def)
let getExn = (d, x) => I.getExn(d.data, x)
