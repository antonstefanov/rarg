open ComponentsErrors;

type runAction('a) = (
  result(Cmd.Action.t, Cmd.Err.t),
  Cmd.t('a),
  Cmd.argsMap,
);

/** follows the sub commands tree and returns the action that should be taken */
let getRunAction = (~cmd: Cmd.t('a), ~argsMap: Cmd.argsMap): runAction('a) => {
  let (cmd, positionals) =
    CmdInternal.findCommandAndPositionals(cmd, ~argsMap);
  let argsMap =
    switch (positionals) {
    | [] => ArgsMap.deletePositionals(argsMap)
    | _ => ArgsMap.setPositionals(argsMap, ~value=Array.of_list(positionals))
    };
  (CmdInternal.getCmdAction(cmd, ~argsMap), cmd, argsMap);
};

module RunResult = {
  type ok('a) =
    | Run('a)
    | Help(string)
    | Version(string)
    | Suggest(string)
    | AutoCompleteScript(string, string)
    | AddPath(string, string)
    | RemovePath;
  type err =
    | ConfigError(string)
    | UserError(string)
    | UnknownError(string);
  type t('a) = result(ok('a), err);
  let configError = err => Error(ConfigError(err));
  let userError = err => Error(UserError(err));
};

let simplify =
    (
      ~runAction: runAction('a),
      ~shell: option(Seed.Process.Shell.t)=?,
      ~platform: option(Seed.Os.Platform.t)=?,
      ~args: array(string),
      ~appName: string,
      ~appPath: string,
      (),
    )
    : RunResult.t('a) => {
  switch (runAction) {
  | (Error(err), cmd, argsMap) =>
    switch (err) {
    | ConfigError(err) =>
      (
        switch (err) {
        | DuplicateArgs(duplicates) => <DuplicateArgs duplicates />
        | InvalidArgNames(invalidArgs) => <InvalidArgNames args=invalidArgs />
        }
      )
      |> RunResult.configError(_)
    | UserError(err) =>
      (
        switch (err) {
        | InvalidArgValues(validationErrors) =>
          <ValidationErrors argsMap validationErrors cmd appName />
        | UnknownArgs(unknownArgs) =>
          <UnknownArgs
            unknownArgs
            definedArgs={cmd.args}
            childCommands={Seed.Option.map(
              cmd.children,
              CmdInternal.Sub.keys,
            )}
          />
        }
      )
      |> RunResult.userError(_)
    }
  | (Ok(action), cmd, argsMap) =>
    switch (action) {
    | Run => Ok(RunResult.Run(cmd.run(argsMap)))
    | Help => Ok(RunResult.Help(<ComponentsHelp.Help cmd appName />))
    | Version => Ok(Version(cmd.version))
    | Suggest(shell) =>
      let suggestions = Suggestions.getSuggestions(~args, ~cmd, ~argsMap);
      let lines = Suggestions.suggestionsForShell(shell, suggestions);
      Ok(RunResult.Suggest(String.concat("\n", lines)));
    | AddPath =>
      Ok(
        RunResult.AddPath(
          <ComponentsTips.AddPathTip appPath ?shell ?platform />,
          <ComponentsTips.AddPathScript appName appPath />,
        ),
      )
    | RemovePath =>
      Error(
        UnknownError(
          "Currently it's not possible to automatically remove path",
        ),
      )
    | AutoCompleteScript =>
      Ok(
        RunResult.AutoCompleteScript(
          <ComponentsTips.AutocompleteTip appName ?shell ?platform />,
          <ComponentsTips.AutocompleteScript appName appPath ?shell />,
        ),
      )
    }
  };
};

module AutorunResult = {
  type ok('a) =
    | Run('a)
    | Handled;
  type err =
    | ConfigError
    | UserError
    | UnknownError;
  type t('a) = result(ok('a), err);
};

let autorun =
    (~logo: option(string)=?, cmd: Cmd.t('a)): AutorunResult.t('a) => {
  let {args, appName, appPath, argsMap}: Argv.t = Argv.make(Sys.argv);
  switch (
    simplify(
      ~runAction=getRunAction(~cmd, ~argsMap),
      ~appName,
      ~appPath,
      ~args,
      (),
    )
  ) {
  | Error(e) =>
    switch (e) {
    | ConfigError(e) =>
      prerr_endline(e);
      Error(ConfigError);
    | UserError(e) =>
      prerr_endline(e);
      Error(UserError);
    | UnknownError(e) =>
      prerr_endline(e);
      Error(UnknownError);
    }
  | Ok(ok) =>
    switch (ok) {
    | Run(a) => Ok(Run(a))
    | Help(str) =>
      switch (logo) {
      | None => ()
      | Some(logo) => print_endline(logo)
      };
      print_endline(str);
      Ok(Handled);
    | Version(version) =>
      print_endline(version);
      Ok(Handled);
    | Suggest(str) =>
      print_endline(str);
      Ok(Handled);
    | AutoCompleteScript(instruction, script)
    | AddPath(instruction, script) =>
      // use stdout to more easily support >> ~/.zshrc
      prerr_endline(instruction);
      print_endline(script);
      Ok(Handled);
    | RemovePath =>
      prerr_endline(
        "Currently it's not possible to automatically remove path",
      );
      Ok(Handled);
    }
  };
};
