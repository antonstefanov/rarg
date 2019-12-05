module Choices = {
  type argsMap = ArgsMap.t;
  type t('a) =
    | HelpAndSuggestions(list('a))
    | Suggestions(list('a))
    | Dynamic((argsMap, (string, array(string))) => list('a));

  let map = (choices: t('a), toB: 'a => 'b): t('b) => {
    switch (choices) {
    | HelpAndSuggestions(xs) => HelpAndSuggestions(List.map(toB, xs))
    | Suggestions(xs) => Suggestions(List.map(toB, xs))
    | Dynamic(predicate) =>
      Dynamic((map, current) => List.map(toB, predicate(map, current)))
    };
  };
};

/** result contains the parsed value or an optional actionable user text */
type parse('a) = string => result('a, option(string));
type t('a) = {
  name: string,
  parse: parse('a),
  stringify: 'a => string,
  choices: option(Choices.t('a)),
};
/**
 * Create a parser
 */
let make = (~name, ~parse, ~stringify, ~choices=?, ()): t('a) => {
  name,
  parse,
  stringify,
  choices,
};

let char: t(char) = {
  name: "char",
  parse: x =>
    switch (String.length(x)) {
    | 1 => Ok(x.[0])
    | _ => Error(None)
    },
  stringify: x => String.make(1, x),
  choices: None,
};

let string: t(string) = {
  name: "string",
  parse: x => Ok(x),
  stringify: x => x,
  choices: None,
};

let int: t(int) = {
  name: "int",
  parse: x => {
    switch (int_of_string(x)) {
    | num => Ok(num)
    | exception _e => Error(None)
    };
  },
  stringify: x => string_of_int(x),
  choices: None,
};

let float: t(float) = {
  name: "float",
  parse: x => {
    switch (float_of_string(x)) {
    | num => Ok(num)
    | exception _e => Error(None)
    };
  },
  stringify: x => string_of_float(x),
  choices: None,
};

let bool: t(bool) = {
  name: "bool",
  parse:
    fun
    | "true" => Ok(true)
    | "false" => Ok(false)
    | _ => Error(None),
  stringify: x => x ? "true" : "false",
  choices: Some(Suggestions([true, false])),
};

let flag: t(bool) = {
  name: "flag",
  parse:
    fun
    | "1"
    | "true"
    | "on"
    | "y"
    | "yes" => Ok(true)
    | "0"
    | "false"
    | "off"
    | "n"
    | "no" => Ok(false)
    | _ => Error(None),
  stringify: x => x ? "true" : "false",
  choices: Some(Suggestions([true, false])),
};

let shell: t(Seed.Process.Shell.t) = {
  name: "shell",
  parse:
    fun
    | "bash" => Ok(Bash)
    | "zsh" => Ok(Zsh)
    | s => Error(Some("Unknown shell: " ++ s)),
  stringify:
    fun
    | Bash => "bash"
    | Zsh => "zsh",
  choices: Some(HelpAndSuggestions([Bash, Zsh])),
};

let withChoices = (t: t('a), choices: Choices.t('a)): t('a) => {
  ...t,
  choices: Some(choices),
};
let with_choices = withChoices;

let branchFilterPattern = branch =>
  String.concat(
    " ",
    [
      "refs/remotes/" ++ branch ++ "*",
      "refs/remotes/" ++ branch ++ "*/**",
      "refs/heads/" ++ branch ++ "*",
      "refs/heads/" ++ branch ++ "*/**",
      "refs/tags/" ++ branch ++ "*",
      "refs/tags/" ++ branch ++ "*/**",
    ],
  );
let branch =
  make(
    ~name="branch",
    ~parse=x => Ok(x),
    ~stringify=x => x,
    ~choices=
      Dynamic(
        (_argsMap, (_key, values)) => {
          let pattern =
            switch (values) {
            | [||] => ""
            | _ => branchFilterPattern(values[Array.length(values) - 1])
            };
          Seed.Chan.readMany(
            {|git for-each-ref --count 30 --format="%(refname:strip=2)" |}
            ++ pattern,
          );
        },
      ),
    (),
  );
