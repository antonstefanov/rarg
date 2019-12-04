open TestFramework;
open Seed;

describe("Ds_Seed_Arr", t =>
  t.describe("slice", t => {
    t.test("returns the same array", t =>
      t.expect.array(Arr.slice([|1, 2, 3, 4|], ~starti=0, ())).toEqual([|
        1,
        2,
        3,
        4,
      |])
    );
    t.test("returns 234", t =>
      t.expect.array(Arr.slice([|1, 2, 3, 4|], ~starti=1, ())).toEqual([|
        2,
        3,
        4,
      |])
    );
    t.test("returns 1", t =>
      t.expect.array(Arr.slice([|1, 2, 3, 4|], ~starti=0, ~endi=1, ())).
        toEqual([|
        1,
      |])
    );
    t.test("returns 12", t =>
      t.expect.array(Arr.slice([|1, 2, 3, 4|], ~starti=0, ~endi=2, ())).
        toEqual([|
        1,
        2,
      |])
    );
    t.test("returns 2", t =>
      t.expect.array(Arr.slice([|1, 2, 3, 4|], ~starti=1, ~endi=2, ())).
        toEqual([|
        2,
      |])
    );
    t.test("returns 23", t =>
      t.expect.array(Arr.slice([|1, 2, 3, 4|], ~starti=1, ~endi=3, ())).
        toEqual([|
        2,
        3,
      |])
    );
    t.test("works with negative end index", t =>
      t.expect.array(Arr.slice([|1, 2, 3, 4|], ~starti=0, ~endi=-1, ())).
        toEqual([|
        1,
        2,
        3,
      |])
    );
    t.test("throws on negative start index", t =>
      t.expect.fn(() => Arr.slice([||], ~starti=-1, ())).toThrow()
    );
    t.test("throws on negative end index when less than start index", t =>
      t.expect.fn(() => Arr.slice([||], ~starti=0, ~endi=-1, ())).toThrow()
    );
  })
);
