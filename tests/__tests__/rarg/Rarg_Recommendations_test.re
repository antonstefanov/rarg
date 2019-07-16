open TestFramework;

module Recommendations = RargInternal.Recommendations;

describe("Rarg_Recommendations", t => {
  t.describe("levenshteinDistance", t => {
    t.test("has 0 distance when same char", t =>
      t.expect.int(Recommendations.levenshteinDistance("a", "a")).toBe(0)
    );
    t.test("has 2 distance when 2 char difference ", t =>
      t.expect.int(Recommendations.levenshteinDistance("a", "zaz")).toBe(2)
    );
    t.test("has 3 distance when 3 char difference ", t =>
      t.expect.int(Recommendations.levenshteinDistance("a", "zzz")).toBe(3)
    );
  });
  t.describe("Internal", t => {
    t.test("includes all entities < threshold", t =>
      t.expect.list(
        Recommendations.Internal.get(
          "a",
          ~candidates=["zxc", "a", "a", "ab", "abc", "abcd", "abcde"],
          (),
        ),
      ).
        toEqual([
        ("a", 0),
        ("ab", 1),
        ("abc", 2),
        ("abcd", 3),
        ("zxc", 3),
      ])
    );
    t.test("sorts first by threshold and then alphabetically", t =>
      t.expect.list(
        Recommendations.Internal.sort([
          ("zyx", 1),
          ("zyx", 4),
          ("abb", 2),
          ("aaa", 2),
          ("abc", 5),
          ("abb", 5),
        ]),
      ).
        toEqual([
        ("zyx", 1),
        ("aaa", 2),
        ("abb", 2),
        ("zyx", 4),
        ("abb", 5),
        ("abc", 5),
      ])
    );
  });
  t.describe("get", t => {
    t.test(
      "includes all entities < threshold when  max returned entities is none",
      t =>
      t.expect.list(
        Recommendations.get(
          "a",
          ~top=99,
          ~candidates=["a", "ab", "abc", "abcd", "abcde"],
          (),
        ),
      ).
        toEqual([
        "a",
        "ab",
        "abc",
        "abcd",
      ])
    );
    t.test("includes the top 3 recommendations < threshold", t =>
      t.expect.list(
        Recommendations.get(
          "a",
          ~top=3,
          ~candidates=["a", "ab", "abc", "abcd", "abcde"],
          (),
        ),
      ).
        toEqual([
        "a",
        "ab",
        "abc",
      ])
    );

    t.test("includes both recommendations", t =>
      t.expect.list(
        Recommendations.get("potat", ~candidates=["potato", "potatoes"], ()),
      ).
        toEqual([
        "potato",
        "potatoes",
      ])
    );
  });
});
