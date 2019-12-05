open TestFramework;

module T = Rarg.Type;
module V = RargInternal.CmdInternal.Validate;
module Args = RargInternal.Args;
module StringMap = Seed.DataStructures.StringMap;

describe("Rarg_CmdInternal_Validate", t => {
  t.describe("checkUnknownArgs", t => {
    t.test("is ok when there are no unknown arguments", t =>
      t.expect.result(
        V.checkUnknownArgs(
          ~isKnown=_ => true,
          ~providedArgs=[("-a", [||])],
        ),
      ).
        toBeOk()
    );
    t.test("is error when there are unknown arguments", t =>
      t.expect.result(
        V.checkUnknownArgs(
          ~isKnown=_ => false,
          ~providedArgs=[("-a", [||])],
        ),
      ).
        toBeError()
    );
    t.test("returns all unknown arguments", t => {
      let result =
        V.checkUnknownArgs(
          ~isKnown=arg => arg == "-b",
          ~providedArgs=[("-a", [||]), ("-b", [||])],
        );
      let err = Seed.Result.getErrExn(result);
      t.expect.list(err).toEqual([("-a", [||])]);
    });
  });
  t.describe("getDuplicatesAndKnownArgs", t => {
    let (args, _) =
      Args.One.req(~args=[], ~name="--v1", ~alias="-v", ~doc="v1", T.string);
    let (duplicatedArgs, _) =
      Args.One.req(~args, ~name="--v1", ~alias="-z", ~doc="v1", T.string);
    t.test("returns a map with both names and aliases", t => {
      let (duplicates, knownArgs) = V.getDuplicatesAndKnownArgs(args);
      t.expect.list(duplicates).toEqual([]);
      t.expect.list(StringMap.keys(knownArgs)).toEqual(["--v1", "-v"]);
    });
    t.test("returns a map with names and aliases when there are duplicates", t => {
      let (duplicates, knownArgs) =
        V.getDuplicatesAndKnownArgs(duplicatedArgs);
      t.expect.list(duplicates).not.toEqual([]);
      t.expect.list(StringMap.keys(knownArgs)).toEqual([
        "--v1",
        "-v",
        "-z",
      ]);
      let (key, _, _) = List.hd(duplicates);
      t.expect.string(key).toEqual("--v1");
    });
  });
  t.describe("isValidArgName", t => {
    t.test("is valid arg name when starting with a dash", t => {
      t.expect.bool(V.isValidArgName("-a")).toBe(true);
      t.expect.bool(V.isValidArgName("--a")).toBe(true);
    });
    t.test("is not a valid arg name when empty", t =>
      t.expect.bool(V.isValidArgName("")).toBe(false)
    );
    t.test("is not a valid arg name when not starting with a dash", t =>
      t.expect.bool(V.isValidArgName("a")).toBe(false)
    );
  });
});
