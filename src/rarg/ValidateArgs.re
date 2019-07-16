module Err = {
  type expected =
    | Eq(int)
    | Gt(int);
  let stringOfExpected =
    fun
    | Eq(expected) => string_of_int(expected)
    | Gt(expected) => string_of_int(expected) ++ " or more";
  let intOfExpected =
    fun
    | Eq(expected) => expected
    | Gt(expected) => expected;

  type argsCount = {
    expected,
    actual: int,
  };
  type transform = {
    actual: string,
    info: option(string),
  };

  type t =
    | ArgsCount(argsCount)
    | Transform(transform);

  let argsCount = (~expected: expected, ~actual: int) =>
    ArgsCount({expected, actual});

  let transform = (~actual: string, ~info: option(string)) =>
    Transform({actual, info});
};

type value = option(array(string));

/**
 * One argument, if the input array contains more than 1 argument - return an error
 */
module One = {
  module Internal = {
    let oneValue = (input: array(string)): option(string) =>
      switch (Array.length(input)) {
      | 1 => Some(input[0])
      | _len => None
      };

    let handleOneArg =
        (parse: Type.parse('a), xs: array(string)): result('a, Err.t) => {
      switch (oneValue(xs)) {
      | None =>
        Error(Err.argsCount(~expected=Eq(1), ~actual=Array.length(xs)))
      | Some(x) =>
        switch (parse(x)) {
        | Error(info) => Error(Err.transform(~actual=x, ~info))
        | Ok(v) => Ok(v)
        }
      };
    };
  };
  let flag =
      (~default: 'a, ~parse: Type.parse('a), value: value)
      : result('a, Err.t) => {
    switch (value) {
    | None => Ok(default)
    | Some(xs) =>
      switch (Array.length(xs)) {
      | 0 => Ok(default)
      | _ => Internal.handleOneArg(parse, xs)
      }
    };
  };
  let req = (~parse: Type.parse('a), value: value): result('a, Err.t) => {
    switch (value) {
    | None => Error(Err.argsCount(~expected=Eq(1), ~actual=0))
    | Some(xs) => Internal.handleOneArg(parse, xs)
    };
  };
  let default =
      (~parse: Type.parse('a), ~default: 'a, value: value)
      : result('a, Err.t) => {
    switch (value) {
    | None => Ok(default)
    | Some(xs) => Internal.handleOneArg(parse, xs)
    };
  };
  let opt =
      (~parse: Type.parse('a), value: value): result(option('a), Err.t) => {
    switch (value) {
    | None => Ok(None)
    | Some(xs) =>
      switch (Internal.handleOneArg(parse, xs)) {
      | Error(err) => Error(err)
      | Ok(v) => Ok(Some(v))
      }
    };
  };
};

/**
 * Many arguments, the input array can contain 0 or more items
 */
module Many = {
  module Internal = {
    let handleTransformMany =
        (~parse: Type.parse('a), arr: array(string))
        : result(list('a), Err.t) => {
      let rec aux = (acc, ~n) => {
        n < 0
          ? Ok(acc)
          : (
            switch (parse(arr[n])) {
            | Error(maybeErr) => Error((arr[n], maybeErr))
            | Ok(v) => aux([v, ...acc], ~n=n - 1)
            }
          );
      };

      switch (aux([], ~n=Array.length(arr) - 1)) {
      | Error((actual, info)) => Error(Err.transform(~actual, ~info))
      | Ok(v) => Ok(v)
      };
    };
  };
  let req = (~parse: Type.parse('a), value: value): result(list('a), Err.t) => {
    switch (value) {
    | None => Error(Err.argsCount(~expected=Gt(1), ~actual=0))
    | Some(xs) => Internal.handleTransformMany(~parse, xs)
    };
  };
  let default =
      (~parse: Type.parse('a), ~default: list('a), value: value)
      : result(list('a), Err.t) => {
    switch (value) {
    | None => Ok(default)
    | Some(xs) => Internal.handleTransformMany(~parse, xs)
    };
  };
  let opt =
      (~parse: Type.parse('a), value: value)
      : result(option(list('a)), Err.t) => {
    switch (value) {
    | None => Ok(None)
    | Some(xs) =>
      switch (Internal.handleTransformMany(~parse, xs)) {
      | Error(err) => Error(err)
      | Ok(v) => Ok(Some(v))
      }
    };
  };
};
