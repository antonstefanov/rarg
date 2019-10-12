// i - index, increases + 1
// n - index, increases - 1

type argType =
  | Short(string)
  | Long(string)
  | Dash
  | Value(string);

type optionType =
  | Short(string)
  | Long(string)
  | Dash;

module StringMap = Seed.DataStructures.StringMap;

type t = StringMap.t(array(string));

let positionalsKey = "<<positionals>>";
let dashKey = "--";
let suggestionsRequestKey = "--rarg-suggestions-request";
let suggestionsScriptKey = "--rarg-suggestions-script";
let addPathKey = "--rarg-add-path";
let removePathKey = "--rarg-remove-path";
let helpKey = "--help";
let versionKey = "--version";
let getOpt = (key: StringMap.key, map: t): option(array(string)) =>
  StringMap.getOpt(key, map);
let getEither =
    (argsMap: t, ~name: string, ~alias: option(string))
    : option(array(string)) =>
  (
    getOpt(name, argsMap),
    Seed.Option.flatMap(alias, ~fn=a => getOpt(a, argsMap)),
  )
  |> Seed.Option.either;
let hasHelp = (map: t): bool => StringMap.has(helpKey, map);
let hasVersion = (map: t): bool => StringMap.has(versionKey, map);
let hasAddPath = (map: t): bool => StringMap.has(addPathKey, map);
let hasRemovePath = (map: t): bool => StringMap.has(removePathKey, map);

let hasSuggestionsScript = (map: t): bool =>
  StringMap.has(suggestionsScriptKey, map);
let getPositionals = (map: t): option(array(string)) =>
  getOpt(positionalsKey, map);
let toList = (map: t): list((string, array(string))) =>
  StringMap.toList(map);
let set = (map: t, ~key: string, ~value: array(string)): t =>
  StringMap.set(key, value, map);
let setPositionals = (map: t, ~value: array(string)): t =>
  set(map, ~key=positionalsKey, ~value);
let deletePositionals = (map: t): t => StringMap.delete(positionalsKey, map);

let getArgType = (str: string): argType => {
  switch (String.length(str)) {
  | 0 => Value(str)
  | 1 => str.[0] == '-' ? Short("-") : Value(str)
  | len =>
    switch (str.[0], str.[1]) {
    | ('-', '-') =>
      switch (len) {
      | 2 => Dash
      | _ => Long(str)
      }
    | ('-', _) => Short(str)
    | (_, _) => Value(str)
    }
  };
};

let rec revTakeUntilOption =
        (args: array(string), ~n): (int, option(optionType)) =>
  n < 0
    ? (0, None)
    : (
      switch (getArgType(args[n])) {
      | Value(_v) => revTakeUntilOption(args, ~n=n - 1)
      | Short(o) => (n, Some(Short(o)))
      | Long(o) => (n, Some(Long(o)))
      | Dash => (n, Some(Dash))
      }
    );

/** performs right to left parsing of arguments and options */
let ofArray = (args: array(string)): t => {
  // append when existing otherwise set
  let add =
      (map: t, ~key: StringMap.key, ~value: array(string))
      : StringMap.t(array(string)) => {
    switch (StringMap.getOpt(key, map)) {
    | None => StringMap.set(key, value, map)
    | Some(existingValue) =>
      StringMap.set(key, Array.append(value, existingValue), map)
    };
  };

  let rec aux = (map, ~endi): t => {
    switch (revTakeUntilOption(args, ~n=endi)) {
    | (_, None) =>
      endi >= 0
        ? add(
            map,
            ~key=positionalsKey,
            ~value=Seed.Arr.slice(args, ~starti=0, ~endi=endi + 1, ()),
          )
        : map
    | (optionIndex, Some(argType)) =>
      switch (argType) {
      | Short(opt) =>
        aux(
          add(
            map,
            ~key=opt,
            ~value=
              Seed.Arr.slice(
                args,
                ~starti=optionIndex + 1,
                ~endi=endi + 1,
                (),
              ),
          ),
          ~endi=optionIndex - 1,
        )
      // TODO: is flag grouping needed?
      // | Short(opt) =>
      //   Seed.Strings.split(opt)
      //   |> List.tl(_)
      //   |> List.fold_left(
      //        (acc, char) =>
      //          add(
      //            acc,
      //            ~key="-" ++ String.make(1, char),
      //            ~value=
      //              Seed.Arr.slice(
      //                args,
      //                ~starti=optionIndex + 1,
      //                ~endi=endi + 1,
      //                (),
      //              ),
      //          ),
      //        map,
      //        _,
      //      )
      //   |> aux(_, ~endi=optionIndex - 1)
      | Long(opt) =>
        aux(
          add(
            map,
            ~key=opt,
            ~value=
              Seed.Arr.slice(
                args,
                ~starti=optionIndex + 1,
                ~endi=endi + 1,
                (),
              ),
          ),
          ~endi=optionIndex - 1,
        )
      | Dash =>
        aux(
          add(
            // reset all parsed options after --
            StringMap.empty,
            ~key=dashKey,
            ~value=Seed.Arr.slice(args, ~starti=optionIndex + 1, ()),
          ),
          ~endi=optionIndex - 1,
        )
      }
    };
  };
  aux(StringMap.empty, ~endi=Array.length(args) - 1);
};

/** split a string to a list of arguments */
let split = (str: string): list(string) => {
  let s = String.trim(str);
  let slen = String.length(s);
  let rec aux = (~prevC, ~ri, ~li, ~opening, ~args): list(string) =>
    if (ri >= slen) {
      [Seed.Strings.slice(s, ~starti=li, ()), ...args];
    } else {
      let char = s.[ri];
      // Seed.Print.arrows(s, ~l=li, ~r=ri);
      switch (char, opening) {
      | (_c, Some(_o)) =>
        aux(
          ~prevC=char,
          ~ri=ri + 1,
          ~li,
          // opening = closing token
          ~opening=Seed.Option.eq(opening, ~v=char) ? None : opening,
          ~args,
        )
      | ('=', None)
      | (' ', None) =>
        aux(
          ~prevC=char,
          ~ri=ri + 1,
          ~li=ri + 1,
          ~opening,
          ~args=
            prevC == ' ' || prevC == '='
              ? args
              : [Seed.Strings.slice(s, ~starti=li, ~endi=ri, ()), ...args],
        )
      | ('\'', None)
      | ('"', None) =>
        aux(~prevC=char, ~ri=ri + 1, ~li, ~opening=Some(char), ~args)
      | (_c, None) => aux(~prevC=char, ~ri=ri + 1, ~li, ~opening, ~args)
      };
    };
  aux(~prevC=' ', ~ri=0, ~li=0, ~opening=None, ~args=[]);
};
