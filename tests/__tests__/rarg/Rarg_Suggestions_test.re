open TestFramework;

module Suggestions = RargInternal.Suggestions;
module ArgsMap = RargInternal.ArgsMap;
module A = RargInternal.Args;
module T = Rarg.Type;
module P = RargInternal.Args.Positional;

type lastArg =
  | Positionals
  | Short
  | Long
  | Dash;

describe("Rarg_Suggestions", t => {
  t.describe("LastArg", t => {
    t.describe("type", t => {
      let last = arr =>
        switch (Suggestions.LastArg.ofArray(arr)) {
        | Positionals(_) => Positionals
        | Short(_) => Short
        | Long(_) => Long
        | Dash(_) => Dash
        };
      t.test("is positional when empty", t =>
        t.expect.equal(last([|""|]), Positionals)
      );
      t.test("is short when a single dash", t =>
        t.expect.equal(last([|"-"|]), Short)
      );
      t.test("is dash when 2 dashes", t =>
        t.expect.equal(last([|"--"|]), Dash)
      );
      t.test("is long when 2 dashes and a char", t =>
        t.expect.equal(last([|"--2"|]), Long)
      );
      t.test("is short when 1 dash and a char", t =>
        t.expect.equal(last([|"-2"|]), Short)
      );
    });
    t.describe("correctness", t => {
      let last = arr =>
        switch (Suggestions.LastArg.ofArray(arr)) {
        | Positionals(v)
        | Short(v)
        | Long(v)
        | Dash(v) => v
        };
      t.test("parses positionals when empty", t => {
        let (name, values) = last([||]);
        t.expect.string(name).toEqual(ArgsMap.positionalsKey);
        t.expect.array(values).toEqual([||]);
      });
      t.test("parses positionals when not empty", t => {
        let (name, values) = last([|"a", "b", "c"|]);
        t.expect.string(name).toEqual(ArgsMap.positionalsKey);
        t.expect.array(values).toEqual([|"a", "b", "c"|]);
      });
      t.test("parses arg when empty values", t => {
        let (name, values) = last([|"--arg"|]);
        t.expect.string(name).toEqual("--arg");
        t.expect.array(values).toEqual([||]);
      });
      t.test("parses arg when not empty", t => {
        let (name, values) = last([|"--arg", "a", "b"|]);
        t.expect.string(name).toEqual("--arg");
        t.expect.array(values).toEqual([|"a", "b"|]);
      });
      t.test("parses only the last arg", t => {
        let (name, values) = last([|"--a", "--b", "--arg", "a", "b"|]);
        t.expect.string(name).toEqual("--arg");
        t.expect.array(values).toEqual([|"a", "b"|]);
      });
    });
  });
  t.describe("getPositionalSuggestions", t => {
    exception
      WrongTestConfiguration(string, array(string), string, array(string));
    let getCurrentArg = (args): (string, array(string)) => {
      switch (Suggestions.LastArg.ofArray(args)) {
      | Positionals(currentArg) => currentArg
      | Short((name, values))
      | Long((name, values))
      | Dash((name, values)) =>
        raise(
          WrongTestConfiguration(
            "Tests in this group should only be for positionals",
            args,
            name,
            values,
          ),
        )
      };
    };
    let get = (definedArgs, children, providedArgs) => {
      let (_, currentValues) = getCurrentArg(providedArgs);
      Suggestions.getPositionalSuggestions(
        ~choices=
          Suggestions.getArgChoices(definedArgs, ~key=ArgsMap.positionalsKey),
        ~children=
          Seed.Option.map(children, ~fn=c =>
            List.map(v => (v, "__" ++ v ++ "__"), c)
          ),
        ~argsMap=ArgsMap.ofArray(providedArgs),
        ~currentValues,
      )
      |> Suggestions.values;
    };
    t.describe("test configuration testing", t => {
      t.test("throws when not positional", t => {
        t.expect.fn(() => getCurrentArg([|"-"|])).toThrow();
        t.expect.fn(() => getCurrentArg([|"--"|])).toThrow();
        t.expect.fn(() => getCurrentArg([|"-a"|])).toThrow();
        t.expect.fn(() => getCurrentArg([|"--a"|])).toThrow();
      });
      t.test("does not throw when positional", t => {
        t.expect.fn(() => getCurrentArg([|"abc"|])).not.toThrow();
        t.expect.fn(() => getCurrentArg([|"a b c"|])).not.toThrow();
      });
    });
    t.test("returns subcommands when no choices", t => {
      let (args, _) = P.One.req(~args=[], ~doc="pos", T.string);
      let result = get(args, Some(["b", "a", "c"]), [|"zz"|]);
      t.expect.list(result).toEqual(["b", "a", "c"]);
    });
    t.test("returns choices when no subcommands", t => {
      let parser = T.(withChoices(string, Suggestions(["b", "a", "c"])));
      let (args, _) = P.One.req(~args=[], ~doc="pos", parser);
      let result = get(args, None, [|"zz"|]);
      t.expect.list(result).toEqual(["b", "a", "c"]);
    });
    t.test("returns choices and subcommands when both are present", t => {
      let parser = T.(withChoices(string, Suggestions(["b", "a", "c"])));
      let (args, _) = P.One.req(~args=[], ~doc="pos", parser);
      let result = get(args, Some(["y", "x", "z"]), [|"zz"|]);
      t.expect.list(result).toEqual(["y", "x", "z", "b", "a", "c"]);
    });
    t.test("includes all choices when word is started", t => {
      let parser = T.(withChoices(string, Suggestions(["apple", "banana"])));
      let (args, _) = P.One.req(~args=[], ~doc="pos", parser);
      let result = get(args, Some(["bake", "slice"]), [|"app"|]);
      t.expect.list(result).toEqual(["bake", "slice", "apple", "banana"]);
    });
    t.test("excludes choice when word is completed", t => {
      let parser = T.(withChoices(string, Suggestions(["apple", "banana"])));
      let (args, _) = P.One.req(~args=[], ~doc="pos", parser);
      let result = get(args, Some(["bake", "slice"]), [|"apple"|]);
      t.expect.list(result).toEqual(["bake", "slice", "banana"]);
    });
    t.test("includes all sub commands when word is started", t => {
      let parser = T.(withChoices(string, Suggestions(["apple", "banana"])));
      let (args, _) = P.One.req(~args=[], ~doc="pos", parser);
      let result = get(args, Some(["bake", "slice"]), [|"ba"|]);
      t.expect.list(result).toEqual(["bake", "slice", "apple", "banana"]);
    });
    t.test("includes all sub commands even when word is completed", t => {
      let parser = T.(withChoices(string, Suggestions(["apple", "banana"])));
      let (args, _) = P.One.req(~args=[], ~doc="pos", parser);
      let result = get(args, Some(["bake", "slice"]), [|"bake", "slice"|]);
      t.expect.list(result).toEqual(["bake", "slice", "apple", "banana"]);
    });
  });
  t.describe("suggestionsForShell", t => {
    t.test("creates a key list for Bash", t => {
      let result =
        Suggestions.suggestionsForShell(
          Bash,
          [("key1", "desc1"), ("key2", "desc2")],
        );
      t.expect.list(result).toEqual(["key1", "key2"]);
    });
    t.test("creates a key:value pair for zsh", t => {
      let result =
        Suggestions.suggestionsForShell(
          Zsh,
          [("key1", "desc1"), ("key2", "desc2")],
        );
      t.expect.list(result).toEqual(["key1:desc1", "key2:desc2"]);
    });
    t.test("takes only the first line of zsh descriptions", t => {
      let result =
        Suggestions.suggestionsForShell(
          Zsh,
          [("key1", "line1\nline2\nline3")],
        );
      t.expect.list(result).toEqual(["key1:line1"]);
    });
  });
  t.describe("getValuesSuggestions", t => {
    exception
      WrongTestConfiguration(string, array(string), string, array(string));
    let getCurrentArg = (args): (string, array(string)) => {
      switch (Suggestions.LastArg.ofArray(args)) {
      | Short(currentArg)
      | Long(currentArg)
      | Dash(currentArg) => currentArg
      | Positionals((name, values)) =>
        raise(
          WrongTestConfiguration(
            "Tests in this group should only be for options and not positionals",
            args,
            name,
            values,
          ),
        )
      };
    };
    let get = (~shell=Seed.Process.Shell.Bash, definedArgs, providedArgs) =>
      Suggestions.getValuesSuggestions(
        ~definedArgs,
        ~argsMap=ArgsMap.ofArray(providedArgs),
        ~currentArg=getCurrentArg(providedArgs),
      )
      |> Suggestions.suggestionsForShell(shell, _);
    t.describe("test configuration testing", t => {
      t.test("throws when positional", t => {
        t.expect.fn(() => getCurrentArg([|""|])).toThrow()
      });
      t.test("does not throw when not positional", t => {
        t.expect.fn(() => getCurrentArg([|"-"|])).not.toThrow();
        t.expect.fn(() => getCurrentArg([|"--"|])).not.toThrow();
        t.expect.fn(() => getCurrentArg([|"-a"|])).not.toThrow();
        t.expect.fn(() => getCurrentArg([|"--arg"|])).not.toThrow();
      });
    });
    t.describe("empty values", t => {
      t.test("is empty when no choices and no sub commands", t => {
        let (args, _) = P.One.req(~args=[], ~doc="pos", T.string);
        let result = get(args, [|"-"|]);
        t.expect.list(result).toEqual([]);
      });
      t.test("returns a list of argument names when single dash", t => {
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", T.string);
        let (args, _) =
          A.One.req(~args, ~name="--slice", ~doc="pos", T.string);
        let result = get(args, [|"-"|]);
        t.expect.list(result).toEqual(["--bake", "--slice"]);
      });
      t.test("returns a list of argument names", t => {
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", T.string);
        let (args, _) =
          A.One.req(~args, ~name="--slice", ~doc="pos", T.string);
        let result = get(args, [|"--"|]);
        t.expect.list(result).toEqual(["--bake", "--slice"]);
      });
      t.test("does not include aliases in suggestions", t => {
        let (args, _) =
          A.One.req(
            ~args=[],
            ~name="--bake",
            ~alias="--alias",
            ~doc="pos",
            T.string,
          );
        let result = get(args, [|"--"|]);
        t.expect.list(result).toEqual(["--bake"]);
      });
      t.test("excludes One args that already have 0 values", t => {
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", T.string);
        let (args, _) =
          A.One.req(~args, ~name="--bake-bread", ~doc="pos", T.string);
        let result = get(args, [|"--bake"|]);
        t.expect.list(result).toEqual(["--bake-bread"]);
      });
      t.test("returns suggestions without the arg", t => {
        let parser =
          T.(withChoices(string, Suggestions(["apple", "banana"])));
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", parser);
        let (args, _) =
          A.One.req(~args, ~name="--slice", ~doc="pos", T.string);
        let result = get(args, [|"--bake"|]);
        t.expect.list(result).toEqual(["apple", "banana", "--slice"]);
      });
      t.test("returns nothing when no arguments", t => {
        let result = get([], [|"--"|]);
        t.expect.list(result).toEqual([]);
      });
    });
    t.describe("provided values", t => {
      t.test("excludes One args that already have 1 value", t => {
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", T.string);
        let (args, _) =
          A.One.req(~args, ~name="--bake-bread", ~doc="pos", T.string);
        let result = get(args, [|"--bake", "apple"|]);
        t.expect.list(result).toEqual(["--bake-bread"]);
      });
      t.test("includes Many args that already have 1 value", t => {
        let (args, _) =
          A.Many.req(~args=[], ~name="--bake", ~doc="pos", T.string);
        let (args, _) =
          A.One.req(~args, ~name="--bake-bread", ~doc="pos", T.string);
        let result = get(args, [|"--bake", "apple"|]);
        t.expect.list(result).toEqual(["--bake", "--bake-bread"]);
      });
      t.test(
        "does not return suggestions if current argument is not the same (bak)",
        t => {
        let parser =
          T.(withChoices(string, Suggestions(["apple", "banana"])));
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", parser);
        let (args, _) =
          A.One.req(~args, ~name="--slice", ~doc="pos", T.string);
        let result = get(args, [|"--bak"|]);
        t.expect.list(result).toEqual(["--bake", "--slice"]);
      });
      t.test("returns the same suggestions even if already provided", t => {
        // TODO: determine if this should change
        let parser =
          T.(withChoices(string, Suggestions(["apple", "banana"])));
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", parser);
        let result = get(args, [|"--bake", "apple"|]);
        t.expect.list(result).toEqual(["apple", "banana"]);
      });
      t.test("returns nothing when no arguments", t => {
        let result = get([], [|"--"|]);
        t.expect.list(result).toEqual([]);
      });
    });
    t.describe("all types", t => {
      t.test("returns suggestions for One.req", t => {
        let parser =
          T.(withChoices(string, Suggestions(["apple", "banana"])));
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", parser);
        let result = get(args, [|"--bake", "apple"|]);
        t.expect.list(result).toEqual(["apple", "banana"]);
      });
      t.test("returns suggestions for One.opt", t => {
        let parser =
          T.(withChoices(string, Suggestions(["apple", "banana"])));
        let (args, _) =
          A.One.req(~args=[], ~name="--bake", ~doc="pos", parser);
        let result = get(args, [|"--bake", "apple"|]);
        t.expect.list(result).toEqual(["apple", "banana"]);
      });
      t.test("returns suggestions for Many.req", t => {
        let parser =
          T.(withChoices(string, Suggestions(["apple", "banana"])));
        let (args, _) =
          A.Many.req(~args=[], ~name="--bake", ~doc="pos", parser);
        let result = get(args, [|"--bake", "apple"|]);
        t.expect.list(result).toEqual(["apple", "banana", "--bake"]);
      });
      t.test("returns suggestions for Many.opt", t => {
        let parser =
          T.(withChoices(string, Suggestions(["apple", "banana"])));
        let (args, _) =
          A.Many.opt(~args=[], ~name="--bake", ~doc="pos", parser);
        let result = get(args, [|"--bake", "apple"|]);
        t.expect.list(result).toEqual(["apple", "banana", "--bake"]);
      });
    });
  });
});
