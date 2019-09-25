open TestFramework;

module CmdInternal = RargInternal.CmdInternal;
module ArgsMap = RargInternal.ArgsMap;
module Args = RargInternal.Args;
module T = Rarg.Type;
module Commands = Rarg_FakeCommands;
module Cmd = Rarg.Cmd;

module R = {
  module Ok = {
    type t =
      | Run
      | Help
      | Version
      | Suggest
      | AutoCompleteScript
      | AddPath
      | RemovePath;
    let stringify =
      fun
      | Run => "Run"
      | Help => "Help"
      | Version => "Version"
      | Suggest => "Suggest"
      | AutoCompleteScript => "AutoCompleteScript"
      | AddPath => "AddPath"
      | RemovePath => "RemovePath";
    let ofCmd = (a: CmdInternal.Action.t) =>
      switch (a) {
      | Run => Run
      | Help => Help
      | Version => Version
      | Suggest => Suggest
      | AutoCompleteScript => AutoCompleteScript
      | AddPath => AddPath
      | RemovePath => RemovePath
      };
    let ofResult = Seed.Result.getOkExn;
  };
  module Err = {
    type err =
      | InvalidArgValues
      | UnknownArgs
      | DuplicateArgs
      | InvalidArgNames;
    let ofCmd = (e: CmdInternal.Err.t): err =>
      switch (e) {
      | ConfigError(e) =>
        switch (e) {
        | DuplicateArgs(_) => DuplicateArgs
        | InvalidArgNames(_) => InvalidArgNames
        }
      | UserError(e) =>
        switch (e) {
        | InvalidArgValues(_) => InvalidArgValues
        | UnknownArgs(_) => UnknownArgs
        }
      };

    let ofResult = Seed.Result.getErrExn;
    exception WrongErrType(string, string);
    let invalidArgValues = (err: CmdInternal.Err.t) =>
      switch (err) {
      | UserError(err) =>
        switch (err) {
        | InvalidArgValues(e) => e
        | UnknownArgs(_e) =>
          raise(WrongErrType("InvalidArgValues", "UnknownArgs"))
        }
      | ConfigError(err) =>
        switch (err) {
        | DuplicateArgs(_e) =>
          raise(WrongErrType("InvalidArgValues", "DuplicateArgs"))
        | InvalidArgNames(_e) =>
          raise(WrongErrType("InvalidArgValues", "InvalidArgNames"))
        }
      };
    let unknownArgs = (err: CmdInternal.Err.t) =>
      switch (err) {
      | UserError(err) =>
        switch (err) {
        | InvalidArgValues(_e) =>
          raise(WrongErrType("UnknownArgs", "InvalidArgValues"))
        | UnknownArgs(e) => e
        }
      | ConfigError(err) =>
        switch (err) {
        | DuplicateArgs(_e) =>
          raise(WrongErrType("UnknownArgs", "DuplicateArgs"))
        | InvalidArgNames(_e) =>
          raise(WrongErrType("UnknownArgs", "InvalidArgNames"))
        }
      };
    let duplicateArgs = (err: CmdInternal.Err.t) =>
      switch (err) {
      | UserError(err) =>
        switch (err) {
        | InvalidArgValues(_e) =>
          raise(WrongErrType("DuplicateArgs", "InvalidArgValues"))
        | UnknownArgs(_e) =>
          raise(WrongErrType("DuplicateArgs", "UnknownArgs"))
        }
      | ConfigError(err) =>
        switch (err) {
        | DuplicateArgs(e) => e
        | InvalidArgNames(_e) =>
          raise(WrongErrType("DuplicateArgs", "InvalidArgNames"))
        }
      };
    let invalidArgNames = (err: CmdInternal.Err.t) =>
      switch (err) {
      | UserError(err) =>
        switch (err) {
        | InvalidArgValues(_e) =>
          raise(WrongErrType("InvalidArgNames", "InvalidArgValues"))
        | UnknownArgs(_e) =>
          raise(WrongErrType("InvalidArgNames", "UnknownArgs"))
        }
      | ConfigError(err) =>
        switch (err) {
        | DuplicateArgs(_e) =>
          raise(WrongErrType("InvalidArgNames", "DuplicateArgs"))
        | InvalidArgNames(e) => e
        }
      };
  };
};

describe("Rarg_CmdInternal", t =>
  t.describe("Internal", t => {
    t.describe("findCommandAndPositionals", t => {
      t.test("returns the first command when no args", t => {
        let (cmd, positionals) =
          CmdInternal.findCommandAndPositionals(
            Commands.CmdStart.cmd,
            ~argsMap=ArgsMap.ofArray([||]),
          );
        t.expect.same(cmd, Commands.CmdStart.cmd);
        t.expect.list(positionals).toEqual([]);
      });
      t.test("returns unrecognized subcommands as positionals", t => {
        let (cmd, positionals) =
          CmdInternal.findCommandAndPositionals(
            Commands.CmdStart.cmd,
            ~argsMap=ArgsMap.ofArray([|"unknown-subcommand"|]),
          );
        t.expect.same(cmd, Commands.CmdStart.cmd);
        t.expect.list(positionals).toEqual(["unknown-subcommand"]);
      });
      //        +-> fruits
      //        |
      // start  +-> car +---> fruit
      //        |
      //        +-> find
      t.test("follows 1st sub-command", t => {
        let (cmd, _positionals) =
          CmdInternal.findCommandAndPositionals(
            Commands.CmdStart.cmd,
            ~argsMap=ArgsMap.ofArray([|"car"|]),
          );
        t.expect.same(cmd, Commands.CmdCar.cmd);
      });
      t.test("follows 2st sub-command", t => {
        let (cmd, _positionals) =
          CmdInternal.findCommandAndPositionals(
            Commands.CmdStart.cmd,
            ~argsMap=ArgsMap.ofArray([|"car", "fruit"|]),
          );
        t.expect.same(cmd, Commands.CmdFruits.cmd);
      });
    });
    t.describe("getCmdAction", t => {
      let get = (cmd, args) =>
        CmdInternal.getCmdAction(cmd, ~argsMap=ArgsMap.ofArray(args));

      t.describe("errors", t => {
        t.describe("priorities", t => {
          t.test("prioritizes duplicate args", t => {
            let (args, _) =
              Args.One.req(~args=[], ~name="--v1", ~doc="v1 doc", T.bool);
            let (args, _) =
              Args.One.req(
                ~args,
                ~name="--v1",
                ~alias="invalid-arg-name",
                ~doc="v1 duplicate doc",
                T.string,
              );
            let cmd =
              Cmd.make(
                ~name="duplicate",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|"--v2", "tru"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.DuplicateArgs);
          });
          t.test("prioritizes invalid arg names", t => {
            let (args, _) =
              Args.One.req(
                ~args=[],
                ~name="--v1",
                ~alias="invalid-arg-name",
                ~doc="v1 duplicate doc",
                T.string,
              );
            let cmd =
              Cmd.make(
                ~name="arg names",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|"--v2", "tru"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.InvalidArgNames);
          });
          t.test("prioritizes unknown args", t => {
            let (args, _) =
              Args.One.req(
                ~args=[],
                ~name="--v1",
                ~doc="v1 duplicate doc",
                T.string,
              );
            let cmd =
              Cmd.make(
                ~name="unknown args",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|"--v2", "tru"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.UnknownArgs);
          });
          t.test("prioritizes invalid args values", t => {
            let (args, _) =
              Args.One.req(
                ~args=[],
                ~name="--v1",
                ~doc="v1 duplicate doc",
                T.bool,
              );
            let cmd =
              Cmd.make(
                ~name="unknown args",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|"--v1", "tru"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.InvalidArgValues);
          });
        });
        t.describe("UnknownArgs", t => {
          t.test("returns an unknown args error for unknown positionals", t => {
            let err = get(Commands.CmdStart.cmd, [|"a"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.UnknownArgs);
            t.expect.list(err |> R.Err.unknownArgs).toEqual([
              (ArgsMap.positionalsKey, [|"a"|]),
            ]);
          });
          t.test("returns an unknown args error for unknown empty arg", t => {
            let err =
              get(Commands.CmdStart.cmd, [|"--unk"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.UnknownArgs);
            t.expect.list(err |> R.Err.unknownArgs).toEqual([
              ("--unk", [||]),
            ]);
          });
          t.test(
            "returns an unknown args error for unknown arg with values", t => {
            let err =
              get(Commands.CmdStart.cmd, [|"--unk", "a", "b"|])
              |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.UnknownArgs);
            t.expect.list(err |> R.Err.unknownArgs).toEqual([
              ("--unk", [|"a", "b"|]),
            ]);
          });
          t.test("returns an unknown args error for unknown empty dash arg", t => {
            let err = get(Commands.CmdStart.cmd, [|"--"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.UnknownArgs);
            t.expect.list(err |> R.Err.unknownArgs).toEqual([("--", [||])]);
          });
          t.test(
            "returns an unknown args error for unknown dash arg with values", t => {
            let err =
              get(Commands.CmdStart.cmd, [|"--", "a", "b"|])
              |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.UnknownArgs);
            t.expect.list(err |> R.Err.unknownArgs).toEqual([
              ("--", [|"a", "b"|]),
            ]);
          });
        });
        t.describe("DuplicateArgs", t =>
          t.test("returns a duplicate args error", t => {
            let (args, _) =
              Args.One.req(~args=[], ~name="--v1", ~doc="v1 doc", T.bool);
            let (args, _) =
              Args.One.req(
                ~args,
                ~name="--v1",
                ~doc="v1 duplicate doc",
                T.string,
              );
            let cmd =
              Cmd.make(
                ~name="duplicate",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|""|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.DuplicateArgs);
            let (a, _) = List.nth(args, 0);
            let (b, _) = List.nth(args, 1);
            t.expect.list(err |> R.Err.duplicateArgs).toEqual([
              ("--v1", a, b),
            ]);
          })
        );
        t.describe("InvalidArgNames", t => {
          t.test("allows positionals", t => {
            let (args, _) =
              Args.Positional.One.req(~args=[], ~doc="v1 doc", T.bool);
            let cmd =
              Cmd.make(
                ~name="positionals",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            t.expect.result(get(cmd, [|"true"|])).toBeOk();
          });
          t.test("returns an invalid args error when empty", t => {
            let (args, _) =
              Args.One.req(~args=[], ~name="", ~doc="v1 doc", T.bool);
            let cmd =
              Cmd.make(
                ~name="empty arg names",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|""|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.InvalidArgNames);
            let (a, _) = List.nth(args, 0);
            t.expect.list(err |> R.Err.invalidArgNames).toEqual([a]);
          });
          t.test(
            "returns an invalid args error when not starting with a dash", t => {
            let (args, _) =
              Args.One.req(
                ~args=[],
                ~name="invalid-arg",
                ~doc="v1 doc",
                T.bool,
              );
            let cmd =
              Cmd.make(
                ~name="non-dash arg names",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|""|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.InvalidArgNames);
            let (a, _) = List.nth(args, 0);
            t.expect.list(err |> R.Err.invalidArgNames).toEqual([a]);
          });
        });
        t.describe("InvalidArgValues", t => {
          t.test("returns invalid arg values when req not provided", t => {
            let (args, _) =
              Args.One.req(~args=[], ~name="--v1", ~doc="v1 doc", T.bool);
            let cmd =
              Cmd.make(
                ~name="invalid args",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|"--v1"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.InvalidArgValues);
          });
          t.test("returns invalid arg values when type is incorrect", t => {
            let (args, _) =
              Args.One.req(~args=[], ~name="--v1", ~doc="v1 doc", T.bool);
            let cmd =
              Cmd.make(
                ~name="invalid args",
                ~version="1.0",
                ~args,
                ~run=_ => (),
                (),
              );
            let err = get(cmd, [|"--v1", "tru"|]) |> R.Err.ofResult;
            t.expect.equal(err |> R.Err.ofCmd, R.Err.InvalidArgValues);
          });
        });
      });
      t.describe("ok", t => {
        t.test("returns ok for empty args", t => {
          let result = get(Commands.CmdStart.cmd, [||]);
          t.expect.result(result).toBeOk();
        });
        t.test("returns suggest when arg is present", t => {
          let ok =
            get(Commands.CmdStart.cmd, [|ArgsMap.suggestionsRequestKey|])
            |> R.Ok.ofResult;
          t.expect.equal(ok |> R.Ok.ofCmd, R.Ok.Suggest);
        });
        t.test("returns add path when arg is present", t => {
          let ok =
            get(Commands.CmdStart.cmd, [|ArgsMap.addPathKey|])
            |> R.Ok.ofResult;
          t.expect.equal(ok |> R.Ok.ofCmd, R.Ok.AddPath);
        });
        t.test("returns remove path when arg is present", t => {
          let ok =
            get(Commands.CmdStart.cmd, [|ArgsMap.removePathKey|])
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.RemovePath);
        });
        t.test("returns a help when arg is present", t => {
          let ok =
            get(Commands.CmdStart.cmd, [|ArgsMap.helpKey|]) |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.Help);
        });
        t.test("returns a version when arg is present", t => {
          let ok =
            get(Commands.CmdStart.cmd, [|ArgsMap.versionKey|])
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.Version);
        });
        t.test("returns an autocomplete when arg is present", t => {
          let ok =
            get(Commands.CmdStart.cmd, [|ArgsMap.suggestionsScriptKey|])
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.AutoCompleteScript);
        });
        t.test("prioritizes suggestions", t => {
          let ok =
            get(
              Commands.CmdStart.cmd,
              [|
                ArgsMap.suggestionsScriptKey,
                ArgsMap.removePathKey,
                ArgsMap.addPathKey,
                ArgsMap.helpKey,
                ArgsMap.versionKey,
                ArgsMap.suggestionsRequestKey,
              |],
            )
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.Suggest);
        });
        t.test("prioritizes autocomplete", t => {
          let ok =
            get(
              Commands.CmdStart.cmd,
              [|
                ArgsMap.suggestionsScriptKey,
                ArgsMap.removePathKey,
                ArgsMap.addPathKey,
                ArgsMap.helpKey,
                ArgsMap.versionKey,
              |],
            )
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.AutoCompleteScript);
        });
        t.test("prioritizes addPath", t => {
          let ok =
            get(
              Commands.CmdStart.cmd,
              [|
                ArgsMap.removePathKey,
                ArgsMap.addPathKey,
                ArgsMap.helpKey,
                ArgsMap.versionKey,
              |],
            )
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.AddPath);
        });
        t.test("prioritizes removePath", t => {
          let ok =
            get(
              Commands.CmdStart.cmd,
              [|ArgsMap.removePathKey, ArgsMap.helpKey, ArgsMap.versionKey|],
            )
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.RemovePath);
        });
        t.test("prioritizes help", t => {
          let ok =
            get(
              Commands.CmdStart.cmd,
              [|ArgsMap.helpKey, ArgsMap.versionKey|],
            )
            |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.Help);
        });
        t.test("returns run when cmd contains no args", t => {
          let args = [];
          let cmd =
            Cmd.make(
              ~name="invalid args",
              ~version="1.0",
              ~args,
              ~run=_ => (),
              (),
            );
          let ok = get(cmd, [||]) |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.Run);
        });
        t.test("returns run when args are valid", t => {
          let args = [];
          let (args, _) =
            Args.One.req(~args, ~name="--v1", ~doc="v1 doc", T.bool);
          let cmd =
            Cmd.make(
              ~name="Args provided",
              ~version="1.0",
              ~args,
              ~run=_ => (),
              (),
            );
          let ok = get(cmd, [|"--v1", "true"|]) |> R.Ok.ofResult;
          t.expect.same(ok |> R.Ok.ofCmd, R.Ok.Run);
        });
      });
    });
  })
);
