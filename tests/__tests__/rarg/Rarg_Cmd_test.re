open TestFramework;
open Rarg;

module A = Rarg.Type;
module Commands = Rarg_FakeCommands;

describe("Rarg_Cmd", t => {
  module C = Rarg.Cmd;
  t.describe("validate", t => {
    let run = _ => ();
    let (args, _) =
      Args.One.req(~args=[], ~name="--v1", ~doc="v1 doc", A.bool);
    let (argsDuplicate, _) =
      Args.One.req(~args, ~name="--v1", ~doc="v1 duplicate doc", A.string);
    let cmd = Cmd.make(~name="cmd", ~args, ~run, ());
    let cmdDuplicate =
      Cmd.make(~name="cmd-duplicate", ~args=argsDuplicate, ~run, ());

    t.test("is ok when cmd is valid", t =>
      t.expect.result(C.validate(cmd)).toBeOk()
    );
    t.test("is error when cmd is invalid", t =>
      t.expect.result(C.validate(cmdDuplicate)).toBeError()
    );
    t.test("is ok when sub cmd is valid", t => {
      let cmdTree =
        Cmd.make(~name="tree", ~args, ~run, ~children=[("valid", cmd)], ());
      t.expect.result(C.validate(cmdTree)).toBeOk();
    });
    t.test("is error when sub cmd is invalid", t => {
      let cmdTree =
        Cmd.make(
          ~name="tree",
          ~args,
          ~run,
          ~children=[("duplicates", cmdDuplicate)],
          (),
        );
      t.expect.result(C.validate(cmdTree)).toBeError();
    });
    t.test("prints validation errors", t => {
      let validateLines =
        Cmd.make(
          ~name="tree",
          ~args,
          ~run,
          ~children=[("duplicates", cmdDuplicate)],
          (),
        )
        |> C.validate(_)
        |> Seed.Result.getErrExn(_)
        |> C.validateErrToString(_);
      t.expect.lines(validateLines).toMatchSnapshot();
    });
  });
});
