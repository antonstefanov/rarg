/**
 * Take the first n elements from a list
 */
let takeTop = (list: list('a), ~top: int): list('a) => {
  let rec aux = (items, acc, i) =>
    i >= top
      ? acc
      : (
        switch (items) {
        | [] => acc
        | [x, ...xs] => aux(xs, [x, ...acc], i + 1)
        }
      );
  aux(list, [], 0) |> List.rev(_);
};

let hasElements = (list: list('a)): bool =>
  switch (list) {
  | [] => false
  | _ => true
  };

let map = (list: list('a), predicate: 'a => 'b): list('b) =>
  List.map(predicate, list);
