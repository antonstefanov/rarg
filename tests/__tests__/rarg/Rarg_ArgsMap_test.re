open TestFramework;

module ArgsMap = RargInternal.ArgsMap;

let makeExpectSome =
    (
      expect: RelyInternal.DefaultMatchers.matchers(unit),
      result,
      arg,
      expected,
    ) =>
  expect.option(ArgsMap.getOpt(arg, result)).toBe(Some(expected));
let makeExpectNone =
    (expect: RelyInternal.DefaultMatchers.matchers(unit), result, arg) =>
  expect.option(ArgsMap.getOpt(arg, result)).toBe(None);

let tsplit = str => ArgsMap.split(str) |> List.rev(_);
let group = args => ArgsMap.ofArray(args) |> ArgsMap.toList(_);

describe("Rarg_ArgsMap", t => {
  t.describe("split", t => {
    t.test("does not throw", t =>
      t.expect.fn(() => ignore(tsplit(""))).not.toThrow()
    );
    t.test("handles unquoted string", t => {
      let args = tsplit("--foo 99");
      t.expect.list(args).toEqual(["--foo", "99"]);
    });
    t.test("handles quoted string with no spaces", t => {
      let args = tsplit("--foo 'hello'");
      t.expect.list(args).toEqual(["--foo", "'hello'"]);
    });
    t.test("handles single quoted string with spaces", t => {
      let args = tsplit("--foo 'hello world' --bar='foo bar'");
      t.expect.list(args).toEqual([
        "--foo",
        "'hello world'",
        "--bar",
        "'foo bar'",
      ]);
    });
    t.test("handles double quoted string with spaces", t => {
      let args = tsplit({|--foo "hello world" --bar="foo bar"|});
      t.expect.list(args).toEqual([
        "--foo",
        {|"hello world"|},
        "--bar",
        {|"foo bar"|},
      ]);
    });
    t.test("handles single quoted empty string", t => {
      let args = tsplit({|--foo '' --bar=''|});
      t.expect.list(args).toEqual(["--foo", "''", "--bar", "''"]);
    });
    t.test("handles double quoted empty string", t => {
      let args = tsplit({|--foo "" --bar=""|});
      t.expect.list(args).toEqual(["--foo", {|""|}, "--bar", {|""|}]);
    });
    t.test("handles quoted string with embeded quotes", t => {
      let args = tsplit({|--foo "hello \'world\'" --bar=\'foo "bar"\'|});
      t.expect.list(args).toEqual([
        "--foo",
        {|"hello \'world\'"|},
        "--bar",
        {|\'foo "bar"\'|},
      ]);
    });
    t.test("ignores unneeded spaces", t => {
      let args = tsplit({|  foo  bar  "foo  bar"  |});
      t.expect.list(args).toEqual(["foo", "bar", {|"foo  bar"|}]);
    });
  });

  t.describe("group", t => {
    t.describe("positionals", t => {
      t.test("contains None positionals", t => {
        let result = ArgsMap.ofArray([|"--foo", "99"|]);
        let expectNone = makeExpectNone(t.expect, result);
        expectNone(ArgsMap.positionalsKey);
      });
      t.test("parses positionals", t => {
        let result = ArgsMap.ofArray([|"a", "b", "--foo", "99"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome(ArgsMap.positionalsKey, [|"a", "b"|]);
      });
    });
    t.describe("short options", t => {
      t.test("parses empty short options", t => {
        let result = ArgsMap.ofArray([|"-f"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("-f", [||]);
      });
      t.test("parses multiple short options", t => {
        let result = ArgsMap.ofArray([|"-f", "a", "b"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("-f", [|"a", "b"|]);
      });
      t.test("parses multi-char flags as one", t => {
        let result = ArgsMap.ofArray([|"-fla", "a", "b"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("-fla", [|"a", "b"|]);
      });
      t.test("does not spread flags", t => {
        let result = ArgsMap.ofArray([|"-fla"|]);
        let expectNone = makeExpectNone(t.expect, result);
        expectNone("-f");
        expectNone("-l");
        expectNone("-a");
      });
    });
    t.describe("long options", t => {
      t.test("parses empty long options", t => {
        let result = ArgsMap.ofArray([|"--foo"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("--foo", [||]);
      });
      t.test("parses long options", t => {
        let result = ArgsMap.ofArray([|"--foo", "a", "b"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("--foo", [|"a", "b"|]);
      });
      t.test("appends multiple values", t => {
        let result = ArgsMap.ofArray([|"--foo", "a", "--bar", "--foo", "b"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("--foo", [|"a", "b"|]);
      });
    });
    t.describe("separator", t => {
      t.test("does not include separator by default", t => {
        let result = ArgsMap.ofArray([|"a", "b"|]);
        let expectNone = makeExpectNone(t.expect, result);
        expectNone("--");
      });
      t.test("parses separator in first position", t => {
        let result = ArgsMap.ofArray([|"--", "a", "b"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("--", [|"a", "b"|]);
      });
      t.test("parses separator", t => {
        let result = ArgsMap.ofArray([|"a", "--", "a", "b"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("--", [|"a", "b"|]);
      });
      t.test("considers options after separator to be regular values", t => {
        let result = ArgsMap.ofArray([|"--", "--flag", "-b", "c"|]);
        let expectSome = makeExpectSome(t.expect, result);
        expectSome("--", [|"--flag", "-b", "c"|]);
      });
    });
    t.describe("mixed", t =>
      t.test("parses", t => {
        let result =
          ArgsMap.ofArray([|
            "a",
            "b",
            "--foo",
            "1",
            "--foo2",
            "2",
            "--bar",
            "-o",
            "-o2",
            "option1",
            "--foo2",
            "3",
            "--zulu",
            "--",
            "-a",
            "--foo",
            "77",
          |]);

        let expectSome = makeExpectSome(t.expect, result);
        expectSome(ArgsMap.positionalsKey, [|"a", "b"|]);
        expectSome("--foo", [|"1"|]);
        expectSome("--foo2", [|"2", "3"|]);
        expectSome("--bar", [||]);
        expectSome("-o", [||]);
        expectSome("-o2", [|"option1"|]);
        expectSome("--zulu", [||]);
        expectSome("--", [|"-a", "--foo", "77"|]);
      })
    );
  });
});
