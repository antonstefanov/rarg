/*
 Translated to reasonml from http://rosettacode.org/wiki/Levenshtein_distance#OCaml
 */
let minimum = (a, b, c) => min(a, min(b, c));

let levenshteinDistance = (s, t) => {
  let m = String.length(s)
  and n = String.length(t);
  /* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t */
  let d = Array.make_matrix(m + 1, n + 1, 0);

  for (i in 0 to m) {
    d[i][0] = i;
  }; /* the distance of any first string to an empty second string */
  for (j in 0 to n) {
    d[0][j] = j;
  }; /* the distance of any second string to an empty first string */

  for (j in 1 to n) {
    for (i in 1 to m)
      {
        if (s.[i - 1] == t.[j - 1]) {
          d[i][j] = d[i - 1][j - 1];
        } else {
          /* no operation required */
          d[i][j] =
            minimum(
              d[i - 1][j] + 1, /* a deletion */
              d[i][j - 1] + 1, /* an insertion */
              d[i - 1][j - 1] + 1,
            );
        };
      }; /* a substitution */
  };

  d[m][n];
};

module S = Seed.DataStructures.StringMap;

module Internal = {
  let get = (str, ~threshold=3, ~candidates, ()): list((string, int)) => {
    let add = (map, ~name, ~distance) => {
      switch (S.getOpt(name, map)) {
      | None => S.set(name, distance, map)
      | Some(existingDistance) when distance < existingDistance =>
        S.set(name, distance, map)
      | Some(_) => map
      };
    };
    List.fold_left(
      (acc, name) =>
        switch (levenshteinDistance(str, name)) {
        | distance when distance <= threshold => add(acc, ~name, ~distance)
        | _ => acc
        },
      S.empty,
      candidates,
    )
    |> S.entries(_);
  };
  let sort = (recommendations: list((string, int))): list((string, int)) =>
    List.sort(
      ((name1, distance1), (name2, distance2)) =>
        switch (distance1 - distance2) {
        | 0 => String.compare(name1, name2)
        | d => d
        },
      recommendations,
    );
};

let get = (str, ~threshold=3, ~top=5, ~candidates, ()): list(string) => {
  let rec aux = (recommendations, ~maxi, ~acc, ~i) =>
    i >= maxi
      ? acc
      : (
        switch (recommendations) {
        | [] => acc
        | [(x, _), ...xs] => aux(xs, ~maxi, ~acc=[x, ...acc], ~i=i + 1)
        }
      );
  Internal.get(str, ~threshold, ~candidates, ())
  |> Internal.sort(_)
  |> aux(_, ~maxi=top, ~acc=[], ~i=0)
  |> List.rev(_);
};

let forArgValues =
    (
      arg: Args.t,
      ~argsMap,
      ~currentArgKey: string,
      ~currentArgValues: array(string),
    )
    : list(string) => {
  switch (
    Suggestions.getChoicesForSuggestions(
      arg.choices,
      ~argsMap,
      ~currentArg=(arg.name, currentArgValues),
    )
  ) {
  | None => []
  | Some(suggestions) =>
    get(currentArgKey, ~threshold=4, ~candidates=suggestions, ())
  };
};

let forArgName =
    (
      ~definedArgs: list((Args.t, 'a)),
      ~argsMap,
      ~currentArg as (current, values): (string, array(string)),
    ) => {
  let suggestions =
    Suggestions.getValuesSuggestions(
      ~definedArgs,
      ~argsMap,
      ~currentArg=(current, values),
    );
  get(current, ~threshold=3, ~candidates=suggestions, ());
};
