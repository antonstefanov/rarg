type run('a) = ArgsMap.t => 'a;

let positionalsKey = ArgsMap.positionalsKey;

module StringMap = Seed.DataStructures.StringMap;
type children('a) = StringMap.t(t('a))
and t('a) = {
  name: string,
  version: string,
  doc: option(string),
  args: list((Args.t, Args.validate)),
  run: run('a),
  children: option(children('a)),
};

module Err = {
  module Config = {
    type t =
      | DuplicateArgs(list((string, Args.t, Args.t)))
      | InvalidArgNames(list(Args.t));
  };
  module User = {
    type t =
      | InvalidArgValues(list((Args.t, ValidateArgs.Err.t)))
      | UnknownArgs(list((string, array(string))));
  };
  type t =
    | ConfigError(Config.t)
    | UserError(User.t);
};

module Validate = {
  let addArg = (~arg, ~name, (duplicates, acc)) => {
    switch (StringMap.getOpt(name, acc)) {
    | None => (duplicates, StringMap.set(name, arg, acc))
    | Some(duplicateArg) => (
        [(name, duplicateArg, arg), ...duplicates],
        acc,
      )
    };
  };

  let getDuplicatesAndKnownArgs = args =>
    List.fold_left(
      ((duplicates, acc), (arg, _)) =>
        switch (Args.(arg.alias)) {
        | None => (duplicates, acc) |> addArg(~arg, ~name=arg.name)
        | Some(alias) =>
          (duplicates, acc)
          |> addArg(~arg, ~name=arg.name)
          |> addArg(~arg, ~name=alias)
        },
      ([], StringMap.empty),
      args,
    );

  let isValidArgName = name =>
    switch (ArgsMap.getArgType(name)) {
    | Value(_) => false
    | Short(_)
    | Long(_)
    | Dash => true
    };
  let checkArgNames =
      (args: list((Args.t, 'a))): result(unit, list(Args.t)) => {
    let invalidArgs =
      List.fold_left(
        (acc, (arg: Args.t, _)) =>
          arg.name == ArgsMap.positionalsKey
          || isValidArgName(arg.name)
          && (
            switch (arg.alias) {
            | None => true
            | Some(alias) => isValidArgName(alias)
            }
          )
            ? acc : [arg, ...acc],
        [],
        args,
      );
    switch (invalidArgs) {
    | [] => Ok()
    | invalidArgs => Error(invalidArgs)
    };
  };

  let definedArgs =
      (args: list((Args.t, Args.validate)))
      : result(StringMap.t(Args.t), Err.Config.t) => {
    let (duplicates, knownArgs) = getDuplicatesAndKnownArgs(args);
    // duplicated args are a configuration mistake -> we check for that first
    switch (duplicates) {
    | [] =>
      switch (checkArgNames(args)) {
      | Error(e) => Error(InvalidArgNames(e))
      | Ok () => Ok(knownArgs)
      }
    | _ => Error(DuplicateArgs(duplicates))
    };
  };

  let checkUnknownArgs =
      (
        ~isKnown: string => bool,
        ~providedArgs: list((string, array(string))),
      )
      : result(unit, list((string, array(string)))) => {
    switch (
      List.fold_left(
        (acc, (key, value)) =>
          isKnown(key) ? acc : [(key, value), ...acc],
        [],
        providedArgs,
      )
    ) {
    | [] => Ok()
    | unknownArgs => Error(unknownArgs)
    };
  };

  let checkKnownArgs =
      (args: list((Args.t, Args.validate)), ~argsMap: ArgsMap.t)
      : result(unit, list((Args.t, ValidateArgs.Err.t))) => {
    switch (
      List.fold_left(
        (acc, (arg, validator)) =>
          switch (validator(argsMap)) {
          | Ok(_) => acc
          | Error(e) => [(arg, e), ...acc]
          },
        [],
        args,
      )
    ) {
    | [] => Ok()
    | errors => Error(errors)
    };
  };
  let providedArgs =
      (
        args: list((Args.t, Args.validate)),
        ~argsMap: ArgsMap.t,
        ~isKnown: string => bool,
      )
      : result(unit, Err.User.t) => {
    switch (
      checkUnknownArgs(~isKnown, ~providedArgs=ArgsMap.toList(argsMap))
    ) {
    | Error(e) => Error(UnknownArgs(e))
    | Ok () =>
      switch (checkKnownArgs(args, ~argsMap)) {
      | Error(e) => Error(InvalidArgValues(e))
      | Ok () => Ok()
      }
    };
  };
};

module Sub = {
  let ofList = (commands: list((StringMap.key, t('a)))): children('a) =>
    StringMap.fromList(commands);
  let getOpt = (key: StringMap.key, map: StringMap.t('a)): option('a) =>
    StringMap.getOpt(key, map);
  let keys = (commands: children('a)): list(string) =>
    StringMap.keys(commands);
};

module Action = {
  type t =
    | Run
    | Help
    | Version
    | Suggest
    | AutoCompleteScript
    | AddPath
    | RemovePath;
};

/** gets the output action for a command without following its subcommands tree */
let getCmdAction =
    (cmd: t('a), ~argsMap: ArgsMap.t): result(Action.t, Err.t) =>
  if (ArgsMap.hasSuggestionsRequest(argsMap)) {
    // autocompletion
    Ok(Action.Suggest);
  } else {
    switch (Validate.definedArgs(cmd.args)) {
    | Error(e) => Error(ConfigError(e))
    | Ok(knownArgs) =>
      ArgsMap.hasSuggestionsScript(argsMap)
        ? Ok(Action.AutoCompleteScript)
        : ArgsMap.hasAddPath(argsMap)
            ? Ok(Action.AddPath)
            : ArgsMap.hasRemovePath(argsMap)
                ? Ok(Action.RemovePath)
                : ArgsMap.hasHelp(argsMap)
                    ? Ok(Action.Help)
                    : ArgsMap.hasVersion(argsMap)
                        ? Ok(Action.Version)
                        : (
                          switch (
                            Validate.providedArgs(
                              cmd.args, ~argsMap, ~isKnown=key =>
                              StringMap.has(key, knownArgs)
                            )
                          ) {
                          | Ok () => Ok(Action.Run)
                          | Error(validationErrors) =>
                            Error(UserError(validationErrors))
                          }
                        )
    };
  };

/** follows the sub commands tree and returns only the positionals that are not commands */
let findCommandAndPositionals =
    (cmd: t('a), ~argsMap: ArgsMap.t): (t('a), list(string)) => {
  let rec aux =
          (cmd: t('a), ~argsMap: ArgsMap.t, ~commands: list(string))
          : (t('a), list(string)) => {
    switch (commands, cmd.children) {
    | ([], _)
    | (_, None) => (cmd, commands)
    | ([command, ...commandsRest], Some(children)) =>
      switch (Sub.getOpt(command, children)) {
      | None => (cmd, commands)
      | Some(subCommand) => aux(subCommand, ~argsMap, ~commands=commandsRest)
      }
    };
  };
  let commands =
    switch (ArgsMap.getPositionals(argsMap)) {
    | None => []
    | Some(positionals) => Array.to_list(positionals)
    };
  aux(cmd, ~argsMap, ~commands);
};
