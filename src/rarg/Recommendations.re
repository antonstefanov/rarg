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
  let get =
      (str, ~threshold=3, ~candidates: list('a), ~getValue: 'a => string, ())
      : list(('a, int)) => {
    let add = (map, ~candidate: 'a, ~value: string, ~distance: int) => {
      switch (S.getOpt(value, map)) {
      | None => S.set(value, (distance, candidate), map)
      | Some((existingDistance, _)) when distance < existingDistance =>
        S.set(value, (distance, candidate), map)
      | Some(_) => map
      };
    };

    let entries =
      List.fold_left(
        (acc, candidate) => {
          let value = getValue(candidate);
          switch (levenshteinDistance(str, value)) {
          | distance when distance <= threshold =>
            add(acc, ~candidate, ~value, ~distance)
          | _ => acc
          };
        },
        S.empty,
        candidates,
      )
      |> S.entries(_);
    List.map(((_, (d, v))) => (v, d), entries);
  };
  let sort = (~compare, recommendations: list(('a, int))): list(('a, int)) =>
    List.sort(
      ((v1, distance1), (v2, distance2)) =>
        switch (distance1 - distance2) {
        | 0 => compare(v1, v2)
        | d => d
        },
      recommendations,
    );
};
// String.compare(name1, name2)
let get =
    (
      str,
      ~threshold=3,
      ~top=5,
      ~candidates: list('a),
      ~compare: ('a, 'a) => int,
      ~getValue: 'a => string,
      (),
    )
    : list('a) => {
  let rec getTop = (recommendations, ~maxi, ~acc, ~i) =>
    i >= maxi
      ? acc
      : (
        switch (recommendations) {
        | [] => acc
        | [(x, _), ...xs] => getTop(xs, ~maxi, ~acc=[x, ...acc], ~i=i + 1)
        }
      );
  Internal.get(str, ~threshold, ~candidates, ~getValue, ())
  |> Internal.sort(_, ~compare)
  |> getTop(_, ~maxi=top, ~acc=[], ~i=0)
  |> List.rev(_);
};

let forArgValues =
    (
      arg: Args.t,
      ~argsMap,
      ~currentArgKey: string,
      ~currentArgValues: array(string),
    )
    : list((string, string)) => {
  switch (
    Suggestions.getChoicesForSuggestions(
      arg.choices,
      ~argsMap,
      ~currentArg=(arg.name, currentArgValues),
    )
  ) {
  | None => []
  | Some(suggestions) =>
    get(
      currentArgKey,
      ~threshold=4,
      ~candidates=List.map(c => (c, ""), suggestions),
      ~getValue=Suggestions.getValue,
      ~compare=((name1, _), (name2, _)) => String.compare(name1, name2),
      (),
    )
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
  get(
    current,
    ~threshold=3,
    ~candidates=suggestions,
    ~getValue=Suggestions.getValue,
    ~compare=((name1, _), (name2, _)) => String.compare(name1, name2),
    (),
  );
};
