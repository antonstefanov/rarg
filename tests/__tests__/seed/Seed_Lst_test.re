open TestFramework;
open Seed;

describe("Rarg_Seed_Lst", t =>
  t.describe("takeTop", t => {
    t.test("returns no elements when negative", t =>
      t.expect.list(Lst.takeTop([1, 2, 3, 4], ~top=-99)).toEqual([])
    );
    t.test("returns no elements when 0", t =>
      t.expect.list(Lst.takeTop([1, 2, 3, 4], ~top=0)).toEqual([])
    );
    t.test("returns the top 2 elements", t =>
      t.expect.list(Lst.takeTop([1, 2, 3, 4], ~top=2)).toEqual([1, 2])
    );
    t.test("returns all elements when top > list len", t =>
      t.expect.list(Lst.takeTop([1, 2, 3, 4], ~top=99)).toEqual([
        1,
        2,
        3,
        4,
      ])
    );
  })
);
