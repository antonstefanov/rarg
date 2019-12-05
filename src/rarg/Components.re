module Span =
  Pastel.Make({});

let isSome = Seed.Option.isSome;
let getMaxLength = items =>
  List.fold_left(
    (acc, text) =>
      switch (String.length(text)) {
      | len when len > acc => len
      | _ => acc
      },
    0,
    items,
  );
let leftRightLines =
    (items: list((string, string))): list((string, int, string)) => {
  let maxLength = getMaxLength(List.map(((left, _)) => left, items));
  List.map(
    ((left, right)) => (left, maxLength - String.length(left), right),
    items,
  );
};
let getLeftRightLine = (~startSpace, ~left, ~midSpace, ~right) => {
  let startSpace = Seed.Strings.repeat(" ", ~times=startSpace);
  let midSpace = Seed.Strings.repeat(" ", ~times=midSpace);
  <Span>
    startSpace
    <Span color=White> left </Span>
    midSpace
    <Span> right </Span>
  </Span>;
};
let spacedLeftRightLines = (items: list((string, int, string))) => {
  List.map(
    ((left, midSpace, right)) =>
      getLeftRightLine(~startSpace=2, ~left, ~midSpace=midSpace + 2, ~right),
    items,
  );
};
let tupleLines = (lines: list((string, string))): list(string) =>
  lines |> leftRightLines(_) |> spacedLeftRightLines(_);

let maybeIndent = (items: list(string), indent: int) =>
  indent == 0
    ? items
    : List.map(
        item => Seed.Strings.repeat(" ", ~times=indent) ++ item,
        items,
      );

let renderIfSome = (maybe: option('a), render: 'a => string) =>
  switch (maybe) {
  | None => ""
  | Some(some) => render(some)
  };
let renderIfTrue = (cond: bool, render: unit => string) =>
  cond ? render() : "";

module Lines = {
  let createElement =
      (~children: list(string), ~indent=0, ~marginBottom=0, ()) =>
    String.concat("\n", maybeIndent(children, indent))
    ++ Seed.Strings.repeat("\n", ~times=marginBottom);
};
module Line = {
  let createElement =
      (~children: list(string), ~indent=0, ~marginBottom=0, ()) =>
    String.concat("", maybeIndent(children, indent))
    ++ Seed.Strings.repeat("\n", ~times=marginBottom);
};
module Maybe = {
  let createElement = (~cond: bool, ~children, ()) =>
    switch (cond) {
    | false => ""
    | true => <Span> ...children </Span>
    };
};
module MaybeText = {
  let createElement = (~text: option(string), ~children as _, ()) =>
    switch (text) {
    | None => ""
    | Some(text) => text
    };
};
module NewLine = {
  let createElement = () => "\n";
};
