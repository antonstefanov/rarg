let isSome = (maybe: option('a)): bool =>
  switch (maybe) {
  | None => false
  | Some(_) => true
  };
let map = (maybe: option('a), ~fn: 'a => 'b): option('b) =>
  switch (maybe) {
  | None => None
  | Some(x) => Some(fn(x))
  };

let bi = (maybe: option('a), ~fn: 'a => 'b, ~default: 'b): 'b =>
  switch (maybe) {
  | None => default
  | Some(x) => fn(x)
  };

let flatMap = (maybe: option('a), ~fn: 'a => 'b): 'b =>
  switch (maybe) {
  | None => None
  | Some(x) => fn(x)
  };

let getExn = (maybe: option('a)): 'a =>
  switch (maybe) {
  | None => failwith("argument is null")
  | Some(x) => x
  };

let getDefault = (maybe: option('a), ~default: 'a): 'a =>
  switch (maybe) {
  | None => default
  | Some(x) => x
  };

let getDefaultLazy = (maybe: option('a), ~default: unit => 'a): 'a =>
  switch (maybe) {
  | None => default()
  | Some(x) => x
  };

let eq = (maybe: option('a), ~v: 'a): bool =>
  switch (maybe) {
  | None => false
  | Some(x) => x == v
  };

let either = ((maybeA: option('a), maybeB: option('a))): option('a) =>
  switch (maybeA, maybeB) {
  | (None, None) => None
  | (Some(x), _)
  | (_, Some(x)) => Some(x)
  };
