open TestFramework;

module Args = RargInternal.Args;
module ArgsMap = RargInternal.ArgsMap;
module Run = RargInternal.Run;

let run =
    (
      ~shell: Seed.Process.Shell.t,
      cmd: RargInternal.Cmd.t('a),
      args: list(string),
    ) => {
  let argsArr = Array.of_list([cmd.name, ...args]);
  let {args, appName, appPath, argsMap}: RargInternal.Argv.t =
    RargInternal.Argv.make(argsArr);
  let runResult =
    Run.simplify(
      ~runAction=Run.getRunAction(~cmd, ~argsMap),
      ~args,
      ~appName,
      ~appPath,
      ~shell,
      ~platform=Darwin,
      (),
    );

  switch (runResult) {
  | Ok(ok) => [
      "Ok",
      ...switch (ok) {
         | Run(_) => ["Run"]
         | Help(help) => ["Help", help]
         | Version(version) => ["Version", version]
         | Suggest(suggestions) => ["Suggest", suggestions]
         // autocomplete script depends on environment
         | AutoCompleteScript(script) => ["AutoCompleteScript", script]
         | AddPath(tip, script) => ["AddPath", tip, script]
         | RemovePath => ["RemovePath"]
         },
    ]
  | Error(err) => [
      "Error",
      ...switch (err) {
         | ConfigError(err) => ["ConfigError", err]
         | UserError(err) => ["UserError", err]
         | UnknownError(err) => ["UnknownError", err]
         },
    ]
  };
};
let runFruits = (args: list(string)) =>
  run(~shell=Bash, Rarg_FakeCommands.CmdFruits.cmd, args);
let runFruitsZsh = (args: list(string)) =>
  run(~shell=Zsh, Rarg_FakeCommands.CmdFruits.cmd, args);
let runDuplicateArg = (providedArgs: list(string)) => {
  let cmd = Rarg_FakeCommands.CmdFruits.cmd;
  let args = cmd.args;
  let (args, _) =
    Args.Positional.Many.req(
      ~args,
      ~name="patterns",
      ~doc="Patterns to find",
      Rarg.Type.string,
    );
  let (args, _) =
    Args.Positional.Many.req(
      ~args,
      ~name="patterns2",
      ~doc="Patterns to find",
      Rarg.Type.string,
    );
  let duplicateArgCmd = {...cmd, args};
  run(~shell=Bash, duplicateArgCmd, providedArgs);
};
let runInvalidArgName = (providedArgs: list(string)) => {
  let cmd = Rarg_FakeCommands.CmdFruits.cmd;
  let args = cmd.args;
  let (args, _) =
    Args.Many.req(
      ~args,
      ~name="patterns",
      ~doc="Patterns to find",
      Rarg.Type.string,
    );
  let invalidArgNameCmd = {...cmd, args};
  run(~shell=Bash, invalidArgNameCmd, providedArgs);
};

describe("Rarg_Run", t => {
  t.describe("Ok", t => {
    t.test("Run", t => {
      let result = runFruits(["--fruits", "apple"]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("Help", t => {
      let result = runFruits(["--help"]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("Version", t => {
      let result = runFruits(["--version"]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("Suggest", t => {
      let result = runFruits(["-fru", ArgsMap.suggestionsRequestKey]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("AutoCompleteScript - bash", t => {
      let result = runFruits([ArgsMap.suggestionsScriptKey]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("AutoCompleteScript - zsh", t => {
      let result = runFruitsZsh([ArgsMap.suggestionsScriptKey]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("AddPath", t => {
      let result = runFruits([ArgsMap.addPathKey]);
      t.expect.lines(result).toMatchSnapshot();
    });
  });
  t.describe("Error", t => {
    t.test("RemovePath", t => {
      let result = runFruits([ArgsMap.removePathKey]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("UserError required args not provided", t => {
      let result = runFruits([]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("UserError provided args invalid", t => {
      let result = runFruits(["--fruits", "pineapple"]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("UserError provided args don't exist", t => {
      let result = runFruits(["--fruit"]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("ConfigError duplicate args", t => {
      let result = runDuplicateArg([]);
      t.expect.lines(result).toMatchSnapshot();
    });
    t.test("ConfigError invalid arg name", t => {
      let result = runInvalidArgName([]);
      t.expect.lines(result).toMatchSnapshot();
    });
  });
});
