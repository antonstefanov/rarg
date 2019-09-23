type kind =
  | Req
  | Opt
  | OptDefault(string);

type possibleValues =
  | ZeroOrOne
  | One
  | Many;

type t = {
  // ex: --foo
  name: string,
  // ex: -f for args <..files> for positionals
  alias: option(string),
  // Argument description
  doc: string,
  // bool
  type_: string,
  possibleValues,
  kind,
  choices: option(Type.Choices.t(string)),
};

type validate = ArgsMap.t => result(unit, ValidateArgs.Err.t);

type argValidateTuple('a) = (list((t, validate)), ArgsMap.t => 'a);

let splitPositionalsAndOptionArgs =
    (definedArgs: list((t, 'a))): (option(t), list(t)) =>
  List.fold_left(
    ((pos: option(t), options: list(t)), (arg: t, _)) =>
      arg.name === ArgsMap.positionalsKey
        ? (Some(arg), options) : (pos, [arg, ...options]),
    (None, []),
    definedArgs,
  );

module Internal = {
  let errOnly = (result: result('a, 'b)): result(unit, 'b) =>
    switch (result) {
    | Ok(_) => Ok()
    | Error(e) => Error(e)
    };

  exception ValidateNotCalled;
  let okExn = (result: result('a, 'b)): 'a =>
    switch (result) {
    | Ok(v) => v
    // ValidateNotCalled is a developer mistake and should not be handled
    | Error(_e) => raise(ValidateNotCalled)
    };

  let cmd =
      (
        ~args: list((t, validate)),
        ~arg: t,
        ~get: ValidateArgs.value => result('a, 'b),
      )
      : argValidateTuple('a) => {
    let getOfArgsMap = (argsMap: ArgsMap.t) => {
      get(ArgsMap.getEither(argsMap, ~name=arg.name, ~alias=arg.alias));
    };
    (
      [(arg, argsMap => getOfArgsMap(argsMap) |> errOnly(_)), ...args],
      argsMap => getOfArgsMap(argsMap) |> okExn(_),
    );
  };
};

module One = {
  let flag =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        ~default: 'a,
        ~empty: 'a,
        parser: Type.t('a),
      )
      : argValidateTuple('a) => {
    let arg: t = {
      name,
      alias,
      doc,
      type_: parser.name,
      kind: OptDefault(parser.stringify(default)),
      possibleValues: ZeroOrOne,
      choices:
        Seed.Option.map(parser.choices, ~fn=choices =>
          Type.Choices.map(choices, parser.stringify)
        ),
    };
    Internal.cmd(
      ~args,
      ~arg,
      ~get=ValidateArgs.One.flag(~parse=parser.parse, ~default, ~empty),
    );
  };
  let boolFlag =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        ~default: bool=false,
        ~empty: bool=true,
        parser: Type.t(bool),
      )
      : argValidateTuple('a) =>
    flag(~args, ~name, ~alias?, ~doc, ~default, ~empty, parser);

  let req =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        parser: Type.t('a),
      )
      : argValidateTuple('a) => {
    let arg: t = {
      name,
      alias,
      doc,
      type_: parser.name,
      kind: Req,
      possibleValues: One,
      choices:
        Seed.Option.map(parser.choices, ~fn=choices =>
          Type.Choices.map(choices, parser.stringify)
        ),
    };

    Internal.cmd(
      ~args,
      ~arg,
      ~get=ValidateArgs.One.req(~parse=parser.parse),
    );
  };
  let default =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        ~default: 'a,
        parser: Type.t('a),
      )
      : argValidateTuple('a) => {
    let arg: t = {
      name,
      alias,
      doc,
      type_: parser.name,
      kind: OptDefault(parser.stringify(default)),
      possibleValues: One,
      choices:
        Seed.Option.map(parser.choices, ~fn=choices =>
          Type.Choices.map(choices, parser.stringify)
        ),
    };
    Internal.cmd(
      ~args,
      ~arg,
      ~get=ValidateArgs.One.default(~parse=parser.parse, ~default),
    );
  };
  let opt =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        parser: Type.t('a),
      )
      : argValidateTuple(option('a)) => {
    let arg: t = {
      name,
      alias,
      doc,
      type_: parser.name,
      kind: Opt,
      possibleValues: One,
      choices:
        Seed.Option.map(parser.choices, ~fn=choices =>
          Type.Choices.map(choices, parser.stringify)
        ),
    };
    Internal.cmd(
      ~args,
      ~arg,
      ~get=ValidateArgs.One.opt(~parse=parser.parse),
    );
  };
};

module Many = {
  let listToString = (list: list('a), ~map: 'a => string) =>
    "[ " ++ String.concat(", ", List.map(map, list)) ++ " ]";
  let req =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        parser: Type.t('a),
      )
      : argValidateTuple(list('a)) => {
    let arg: t = {
      name,
      alias,
      doc,
      type_: parser.name,
      kind: Req,
      possibleValues: Many,
      choices:
        Seed.Option.map(parser.choices, ~fn=choices =>
          Type.Choices.map(choices, parser.stringify)
        ),
    };
    Internal.cmd(
      ~args,
      ~arg,
      ~get=ValidateArgs.Many.req(~parse=parser.parse),
    );
  };
  let default =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        ~default: list('a),
        parser: Type.t('a),
      )
      : argValidateTuple(list('a)) => {
    let arg: t = {
      name,
      alias,
      doc,
      type_: parser.name,
      kind: OptDefault(listToString(default, ~map=parser.stringify)),
      possibleValues: Many,
      choices:
        Seed.Option.map(parser.choices, ~fn=choices =>
          Type.Choices.map(choices, parser.stringify)
        ),
    };
    Internal.cmd(
      ~args,
      ~arg,
      ~get=ValidateArgs.Many.default(~parse=parser.parse, ~default),
    );
  };
  let opt =
      (
        ~args: list((t, validate)),
        ~name: string,
        ~alias: option(string)=?,
        ~doc: string,
        parser: Type.t('a),
      )
      : argValidateTuple(option(list('a))) => {
    let arg: t = {
      name,
      alias,
      doc,
      type_: parser.name,
      kind: Opt,
      possibleValues: Many,
      choices:
        Seed.Option.map(parser.choices, ~fn=choices =>
          Type.Choices.map(choices, parser.stringify)
        ),
    };
    Internal.cmd(
      ~args,
      ~arg,
      ~get=ValidateArgs.Many.opt(~parse=parser.parse),
    );
  };
};

module Positional = {
  module One = {
    let flag =
        (
          ~args: list((t, validate)),
          ~name: option(string)=?,
          ~doc: string,
          ~default: 'a,
          parser: Type.t('a),
        )
        : argValidateTuple('a) =>
      One.flag(
        ~args,
        ~name=ArgsMap.positionalsKey,
        ~alias=?name,
        ~doc,
        ~default,
        ~empty=default,
        parser,
      );

    let req =
        (
          ~args: list((t, validate)),
          ~name: option(string)=?,
          ~doc: string,
          parser: Type.t('a),
        )
        : argValidateTuple('a) =>
      One.req(
        ~args,
        ~name=ArgsMap.positionalsKey,
        ~alias=?name,
        ~doc,
        parser,
      );

    let default =
        (
          ~args: list((t, validate)),
          ~name: option(string)=?,
          ~doc: string,
          ~default: 'a,
          parser: Type.t('a),
        )
        : argValidateTuple('a) =>
      One.default(
        ~args,
        ~name=ArgsMap.positionalsKey,
        ~alias=?name,
        ~doc,
        ~default,
        parser,
      );

    let opt =
        (
          ~args: list((t, validate)),
          ~name: option(string)=?,
          ~doc: string,
          parser: Type.t('a),
        )
        : argValidateTuple(option('a)) =>
      One.opt(
        ~args,
        ~name=ArgsMap.positionalsKey,
        ~alias=?name,
        ~doc,
        parser,
      );
  };
  module Many = {
    let req =
        (
          ~args: list((t, validate)),
          ~name: option(string)=?,
          ~doc: string,
          parser: Type.t('a),
        )
        : argValidateTuple(list('a)) =>
      Many.req(
        ~args,
        ~name=ArgsMap.positionalsKey,
        ~alias=?name,
        ~doc,
        parser,
      );

    let default =
        (
          ~args: list((t, validate)),
          ~name: option(string)=?,
          ~doc: string,
          ~default: list('a),
          parser: Type.t('a),
        )
        : argValidateTuple(list('a)) =>
      Many.default(
        ~args,
        ~name=ArgsMap.positionalsKey,
        ~alias=?name,
        ~doc,
        ~default,
        parser,
      );

    let opt =
        (
          ~args: list((t, validate)),
          ~name: option(string)=?,
          ~doc: string,
          parser: Type.t('a),
        )
        : argValidateTuple(option(list('a))) =>
      Many.opt(
        ~args,
        ~name=ArgsMap.positionalsKey,
        ~alias=?name,
        ~doc,
        parser,
      );
  };
};
