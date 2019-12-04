open TestFramework;
open Seed;

let get = Option.getExn;

describe("Ds_Seed_Strings", t => {
  t.describe("repeat", t => {
    t.test("is empty", t =>
      t.expect.string(Strings.repeat("abc", ~times=0)).toEqual("")
    );
    t.test("repeats abc 3 times", t =>
      t.expect.string(Strings.repeat("abc", ~times=3)).toEqual("abcabcabc")
    );
  });
  t.describe("slice", t => {
    t.test("returns the same string", t =>
      t.expect.string(Strings.slice("abcdef", ~starti=0, ())).toEqual(
        "abcdef",
      )
    );
    t.test("returns bcdef", t =>
      t.expect.string(Strings.slice("abcdef", ~starti=1, ())).toEqual(
        "bcdef",
      )
    );
    t.test("returns a", t =>
      t.expect.string(Strings.slice("abcdef", ~starti=0, ~endi=1, ())).
        toEqual(
        "a",
      )
    );
    t.test("returns ab", t =>
      t.expect.string(Strings.slice("abcdef", ~starti=0, ~endi=2, ())).
        toEqual(
        "ab",
      )
    );
    t.test("returns b", t =>
      t.expect.string(Strings.slice("abcdef", ~starti=1, ~endi=2, ())).
        toEqual(
        "b",
      )
    );
    t.test("returns bc", t =>
      t.expect.string(Strings.slice("abcdef", ~starti=1, ~endi=3, ())).
        toEqual(
        "bc",
      )
    );
  });
  t.describe("splitAtIndex", t => {
    t.test("does not include the split index", t => {
      let (left, right) = Strings.splitAtIndex("abcdf", ~index=2);
      t.expect.string(left).toEqual("ab");
      t.expect.string(right).toEqual("df");
    });
    t.test("splits correctly at 0 index", t => {
      let (left, right) = Strings.splitAtIndex("abcdf", ~index=0);
      t.expect.string(left).toEqual("");
      t.expect.string(right).toEqual("bcdf");
    });
    t.test("splits correctly at last index", t => {
      let (left, right) =
        Strings.splitAtIndex("abcdf", ~index=String.length("abcdf") - 1);
      t.expect.string(left).toEqual("abcd");
      t.expect.string(right).toEqual("");
    });
    t.test("throws if index is outside the boundaries", t =>
      t.expect.fn(() => Strings.splitAtIndex("abcdf", ~index=99)).toThrow()
    );
  });
  t.describe("splitAtChar", t => {
    t.test("returns none if char does not exist", t => {
      let result = Strings.splitAtChar("abcdf", ~char='z');
      t.expect.option(result).toBeNone();
    });
    t.test("returns some if char exists", t => {
      let result = Strings.splitAtChar("abcdf", ~char='c');
      t.expect.option(result).toBeSome();
    });
    t.test("does not include the split char", t => {
      let (left, right) = Strings.splitAtChar("abcdf", ~char='c') |> get;
      t.expect.string(left).toEqual("ab");
      t.expect.string(right).toEqual("df");
    });
    t.test("splits correctly when char is at 0 index", t => {
      let (left, right) = Strings.splitAtChar("abcdf", ~char='a') |> get;
      t.expect.string(left).toEqual("");
      t.expect.string(right).toEqual("bcdf");
    });
    t.test("splits correctly when char is at last index", t => {
      let (left, right) = Strings.splitAtChar("abcdf", ~char='f') |> get;
      t.expect.string(left).toEqual("abcd");
      t.expect.string(right).toEqual("");
    });
  });
  t.describe("split", t => {
    t.test("returns empty list when string is empty", t => {
      let result = Strings.split("");
      t.expect.list(result).toEqual([]);
    });
    t.test("splits string on chars", t => {
      let result = Strings.split("abcdf");
      t.expect.list(result).toEqual(['a', 'b', 'c', 'd', 'f']);
    });
  });
});
