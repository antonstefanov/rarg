open TestFramework;

module A = RargInternal.ValidateArgs;

module V = {
  let empty = [];
  let string = ["string-value"];
  let strings = ["v1", "v2"];
  let one = string;
  let many = strings;
  let chars = ["c", "1", "$", "*"];
  let ints = ["3", "4"];
  let floats = ["3.4", "4.5"];
  let bools = ["true", "false"];
  let flags = ["true", "false", "0", "1", "on", "off", "yes", "no", "y", "n"];
};
module O = {
  let none = None;
  let empty = Some(V.empty |> Array.of_list);
  let string = Some(V.string |> Array.of_list);
  let strings = Some(V.strings |> Array.of_list);
  let one = string;
  let many = strings;
  let chars = Some(V.chars |> Array.of_list);
  let ints = Some(V.ints |> Array.of_list);
  let floats = Some(V.floats |> Array.of_list);
  let bools = Some(V.bools |> Array.of_list);
  let flags = Some(V.flags |> Array.of_list);
};

describe("Rarg_ValidateArgs", t => {
  t.describe("One", t => {
    let parse = str => Ok(str);
    let default = "default-value";
    let empty = "empty-value";
    let oneValue = List.hd(V.one);
    t.describe("flag", t => {
      t.test("returns the default value when no value is provided", t =>
        t.expect.result(A.One.flag(~parse, ~default, ~empty, O.none)).toBe(
          Ok(default),
        )
      );
      t.test("returns the empty value for empty values", t =>
        t.expect.result(A.One.flag(~parse, ~default, ~empty, O.empty)).toBe(
          Ok(empty),
        )
      );
      t.test("returns an error for required one, but provided many values", t =>
        t.expect.result(A.One.flag(~parse, ~default, ~empty, O.many)).
          toBeError()
      );
      t.test("returns ok when one value is provided", t =>
        t.expect.result(A.One.flag(~parse, ~default, ~empty, O.one)).toBe(
          Ok(oneValue),
        )
      );
    });
    t.describe("req", t => {
      t.test("returns an error for required, but missing values", t =>
        t.expect.result(A.One.req(~parse, O.none)).toBeError()
      );
      t.test("returns an error for required, but empty values", t =>
        t.expect.result(A.One.req(~parse, O.empty)).toBeError()
      );
      t.test("returns an error for required one, but provided many values", t =>
        t.expect.result(A.One.req(~parse, O.many)).toBeError()
      );
      t.test("returns ok when one value is provided", t =>
        t.expect.result(A.One.req(~parse, O.one)).toBe(Ok(oneValue))
      );
    });
    t.describe("opt", t => {
      t.test("returns none for missing values", t =>
        t.expect.result(A.One.opt(~parse, O.none)).toBe(Ok(None))
      );
      t.test("returns an error for optional, but empty values", t =>
        t.expect.result(A.One.opt(~parse, O.empty)).toBeError()
      );
      t.test("returns an error for optional one, but provided many values", t =>
        t.expect.result(A.One.opt(~parse, O.many)).toBeError()
      );
      t.test("returns ok when one value is provided", t =>
        t.expect.result(A.One.opt(~parse, O.one)).toBe(Ok(Some(oneValue)))
      );
    });
    t.describe("default", t => {
      let default = "default-value";
      t.test("returns the default value when no value is provided", t =>
        t.expect.result(A.One.default(~parse, ~default, O.none)).toBe(
          Ok(default),
        )
      );
      t.test("returns an error for empty values", t =>
        t.expect.result(A.One.default(~parse, ~default, O.empty)).toBeError()
      );
      t.test("returns an error for required one, but provided many values", t =>
        t.expect.result(A.One.default(~parse, ~default, O.many)).toBeError()
      );
      t.test("returns ok when one value is provided", t =>
        t.expect.result(A.One.default(~parse, ~default, O.one)).toBe(
          Ok(oneValue),
        )
      );
    });
  });

  t.describe("Many", t => {
    let parse = str => Ok(str);
    t.describe("manyReq", t => {
      t.test("returns an error for required, but missing values", t =>
        t.expect.result(A.Many.req(~parse, O.none)).toBeError()
      );
      t.test("returns an empty list for required, but empty values", t =>
        t.expect.result(A.Many.req(~parse, O.empty)).toBe(Ok([]))
      );
      t.test("returns ok for required many, but provided one value", t =>
        t.expect.result(A.Many.req(~parse, O.one)).toBe(Ok(V.one))
      );
      t.test("returns ok when many values are provided", t =>
        t.expect.result(A.Many.req(~parse, O.many)).toBe(Ok(V.many))
      );
    });
    t.describe("manyOpt", t => {
      t.test("returns none for missing values", t =>
        t.expect.result(A.Many.opt(~parse, O.none)).toBe(Ok(None))
      );
      t.test("returns an empty list for required, but empty values", t =>
        t.expect.result(A.Many.opt(~parse, O.empty)).toBe(Ok(Some([])))
      );
      t.test("returns ok for optional many, but provided one value", t =>
        t.expect.result(A.Many.opt(~parse, O.one)).toBe(Ok(Some(V.one)))
      );
      t.test("returns ok when many values are provided", t =>
        t.expect.result(A.Many.opt(~parse, O.many)).toBe(Ok(Some(V.many)))
      );
    });
    t.describe("manyOptDefault", t => {
      let default = ["default-value-1", "default-value-2"];
      t.test("returns ok when many values are provided", t =>
        t.expect.result(A.Many.req(~parse, O.many)).toBe(Ok(V.many))
      );
      t.test("returns the default value when no value is provided", t =>
        t.expect.result(A.Many.default(~parse, ~default, O.none)).toBe(
          Ok(default),
        )
      );
      t.test("returns an empty list for required, but empty values", t =>
        t.expect.result(A.Many.default(~parse, ~default, O.empty)).toBe(
          Ok([]),
        )
      );
      t.test("returns ok for required many, but provided one value", t =>
        t.expect.result(A.Many.default(~parse, ~default, O.one)).toBe(
          Ok(V.one),
        )
      );
      t.test("returns ok when many values are provided", t =>
        t.expect.result(A.Many.default(~parse, ~default, O.many)).toBe(
          Ok(V.many),
        )
      );
    });
  });

  t.describe("Char", t => {
    let parse = Rarg.Type.char.parse;
    t.test("parses", t =>
      t.expect.result(A.Many.req(~parse, O.chars)).toBe(
        Ok(['c', '1', '$', '*']),
      )
    );
    t.test("returns an error when input is invalid", t =>
      t.expect.result(A.Many.req(~parse, O.strings)).toBeError()
    );
  });
  t.describe("String", t => {
    let parse = Rarg.Type.string.parse;
    t.test("returns the same string", t =>
      t.expect.result(A.One.req(~parse, O.string)).toBe(Ok("string-value"))
    );
  });
  t.describe("Int", t => {
    let parse = Rarg.Type.int.parse;
    t.test("parses", t =>
      t.expect.result(A.Many.req(~parse, O.ints)).toBe(Ok([3, 4]))
    );
    t.test("returns an error when input is strings", t =>
      t.expect.result(A.Many.req(~parse, O.strings)).toBeError()
    );
    t.test("returns an error when input is floats", t =>
      t.expect.result(A.Many.req(~parse, O.floats)).toBeError()
    );
  });
  t.describe("Float", t => {
    let parse = Rarg.Type.float.parse;
    t.test("parses", t =>
      t.expect.result(A.Many.req(~parse, O.floats)).toBe(Ok([3.4, 4.5]))
    );
    t.test("parses ints as floats", t =>
      t.expect.result(A.Many.req(~parse, O.ints)).toBe(Ok([3.0, 4.0]))
    );
    t.test("returns an error when input is strings", t =>
      t.expect.result(A.Many.req(~parse, O.strings)).toBeError()
    );
  });
  t.describe("Bool", t => {
    let parse = Rarg.Type.bool.parse;
    t.test("parses", t =>
      t.expect.result(A.Many.req(~parse, O.bools)).toBe(Ok([true, false]))
    );
    t.test("returns an error when input is strings", t =>
      t.expect.result(A.Many.req(~parse, O.strings)).toBeError()
    );
    t.test("returns an error when input is flags", t =>
      t.expect.result(A.Many.req(~parse, O.flags)).toBeError()
    );
  });
  t.describe("Flag", t => {
    let parse = Rarg.Type.flag.parse;
    t.test("parses", t =>
      t.expect.result(A.Many.req(~parse, O.flags)).toBe(
        // [|"true", "false", "0", "1", "on", "off", "yes", "no", "y", "n"|],
        Ok([true, false, false, true, true, false, true, false, true, false]),
      )
    );
    t.test("returns an error when input is strings", t =>
      t.expect.result(A.Many.req(~parse, O.strings)).toBeError()
    );
  });
});
