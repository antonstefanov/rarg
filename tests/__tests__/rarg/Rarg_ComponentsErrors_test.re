open TestFramework;
open RargInternal.ComponentsErrors;

module Args = RargInternal.Args;
module ArgsMap = RargInternal.ArgsMap;
module E = RargInternal.ValidateArgs.Err;

module A = Rarg.Type;
let argBake = {
  let (args, _) =
    Args.One.boolFlag(
      ~args=[],
      ~name="--bake",
      ~doc="Whether to bake the fruit",
      A.bool,
    );
  let (arg, _) = List.hd(args);
  arg;
};

describe("Rarg_ComponentsErrors", t => {
  t.describe("DuplicateArgs", t => {
    t.test("renders duplicates when 1", t =>
      t.expect.string(
        <DuplicateArgs duplicates=[("--key1", argBake, argBake)] />,
      ).
        toMatchSnapshot()
    );
    t.test("renders duplicates when 2", t =>
      t.expect.string(
        <DuplicateArgs
          duplicates=[
            ("--key1", argBake, argBake),
            ("--key2", argBake, argBake),
          ]
        />,
      ).
        toMatchSnapshot()
    );
  });
  t.describe("InvalidArgNames", t => {
    let argEmpty = {
      let (args, _) =
        Args.One.boolFlag(
          ~args=[],
          ~name="",
          ~doc="Whether to bake the fruit",
          A.bool,
        );
      let (arg, _) = List.hd(args);
      arg;
    };
    let argNoDash = {
      let (args, _) =
        Args.One.boolFlag(
          ~args=[],
          ~name="invalid-arg-name",
          ~doc="Whether to bake the fruit",
          A.bool,
        );
      let (arg, _) = List.hd(args);
      arg;
    };
    t.test("renders empty arg names", t =>
      t.expect.string(<InvalidArgNames args=[argEmpty] />).toMatchSnapshot()
    );
    t.test("renders no-dash arg names", t =>
      t.expect.string(<InvalidArgNames args=[argNoDash] />).toMatchSnapshot()
    );
  });
  t.describe("ValidationError", t => {
    let arg = argBake;
    // argsMap is used only for dynamic suggestions
    let argsMap = ArgsMap.ofArray([||]);
    t.describe("ArgsCount", t => {
      t.test("shows expected=3 received=5", t => {
        let validateErr = E.ArgsCount({expected: Eq(3), actual: 5});
        t.expect.string(<ValidationError arg validateErr argsMap />).
          toMatchSnapshot();
      });
      t.test("shows expected>=3 received=5", t => {
        // does not check that actual 5 is > 3
        let validateErr = E.ArgsCount({expected: Gt(3), actual: 5});
        t.expect.string(<ValidationError arg validateErr argsMap />).
          toMatchSnapshot();
      });
    });
    t.describe("ArgsCount", t => {
      t.test("shows true recommendation when no info", t => {
        let validateErr = E.Transform({actual: "tr", info: None});
        t.expect.string(<ValidationError arg validateErr argsMap />).
          toMatchSnapshot();
      });
      t.test("shows true recommendation when info", t => {
        let validateErr =
          E.Transform({actual: "tr", info: Some("Some info")});
        t.expect.string(<ValidationError arg validateErr argsMap />).
          toMatchSnapshot();
      });
    });
  });
  t.describe("UnknownArgs", t => {
    let args = [];
    let (args, _) =
      Args.One.boolFlag(
        ~args,
        ~name="--bake",
        ~doc="Whether to bake the fruit",
        A.bool,
      );
    let (args, _) =
      Args.One.req(~args, ~name="--cake", ~doc="The cake to bake", A.string);
    let definedArgsNoPositionals = args;
    let (definedArgsPositionals, _) =
      Args.Positional.One.req(~args, ~doc="Positionals", A.string);
    t.describe("no positionals", t => {
      let unknownArgs = [
        ("cak", [|"unknown-value1", "unknown-value2"|]),
        ("--bak", [|"uv1", "uv2"|]),
      ];
      t.test(
        "renders arg key suggestions when no subcommands, no positionals", t =>
        t.expect.string(
          <UnknownArgs
            unknownArgs
            definedArgs=definedArgsNoPositionals
            childCommands=None
          />,
        ).
          toMatchSnapshot()
      );
      t.test("renders arg key suggestions when subcommands, positionals", t =>
        t.expect.string(
          <UnknownArgs
            unknownArgs
            definedArgs=definedArgsPositionals
            childCommands={Some(["sub1"])}
          />,
        ).
          toMatchSnapshot()
      );
    });
    t.describe("with positionals", t => {
      let unknownArgs = [
        ("cak", [|"unknown-value1", "unknown-value2"|]),
        (ArgsMap.positionalsKey, [|"tomat", "potat"|]),
        ("--bak", [|"uv1", "uv2"|]),
      ];
      t.test("throws when unknown positionals, but has positionals", t =>
        t.expect.fn(() =>
          <UnknownArgs
            unknownArgs
            definedArgs=definedArgsPositionals
            childCommands=None
          />
        ).
          toThrow()
      );
      t.test("renders suggestions when no subcommands, no positionals", t =>
        t.expect.string(
          <UnknownArgs
            unknownArgs
            definedArgs=definedArgsNoPositionals
            childCommands=None
          />,
        ).
          toMatchSnapshot()
      );
      t.test("renders suggestions when there's no good subcommands match", t =>
        t.expect.string(
          <UnknownArgs
            unknownArgs
            definedArgs=definedArgsNoPositionals
            childCommands={Some(["cake"])}
          />,
        ).
          toMatchSnapshot()
      );
      t.test(
        "prioritizes suggestions based on the first positional - tomat",
        // suggestions are based on the first positional and will prioritize tom,
        // even though potato is a more accurate suggestion for potat
        t =>
        t.expect.string(
          <UnknownArgs
            unknownArgs
            definedArgs=definedArgsNoPositionals
            childCommands={Some(["potato", "tom"])}
          />,
        ).
          toMatchSnapshot()
      );
    });
  });
});
