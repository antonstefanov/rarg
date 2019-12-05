module LastArg = {
  type lastArg = (string, array(string));
  type t =
    | Positionals(lastArg)
    | Short(lastArg)
    | Long(lastArg)
    | Dash(lastArg);

  let ofArray = (args: array(string)): t => {
    let endi = Array.length(args) - 1;
    switch (ArgsMap.revTakeUntilOption(args, ~n=endi)) {
    | (_, None) =>
      Positionals((
        ArgsMap.positionalsKey,
        Seed.Arr.slice(args, ~starti=0, ~endi=endi + 1, ()),
      ))
    | (optionIndex, Some(argType)) =>
      switch (argType) {
      | Short(opt) =>
        Short((
          opt,
          Seed.Arr.slice(args, ~starti=optionIndex + 1, ~endi=endi + 1, ()),
        ))
      | Long(opt) =>
        Long((
          opt,
          Seed.Arr.slice(args, ~starti=optionIndex + 1, ~endi=endi + 1, ()),
        ))
      | Dash =>
        Dash((
          ArgsMap.dashKey,
          Seed.Arr.slice(args, ~starti=optionIndex + 1, ()),
        ))
      }
    };
  };
};

let getChoicesForSuggestions =
    (
      choices: option(Type.Choices.t('a)),
      ~argsMap: ArgsMap.t,
      ~currentArg: (string, array(string)),
    )
    : option(list('a)) =>
  Seed.Option.flatMap(choices, ~fn=choices =>
    switch (choices) {
    | HelpAndSuggestions(xs)
    | Suggestions(xs) => Some(xs)
    | Dynamic(predicate) => Some(predicate(argsMap, currentArg))
    }
  );

// only show arg name suggestions if arg not already provided
// or if many can be provided
let showArgNameSuggestions = (arg: Args.t, ~argsMap) => {
  switch (arg.possibleValues) {
  | ZeroOrOne
  | One =>
    switch (ArgsMap.getEither(argsMap, ~name=arg.name, ~alias=arg.alias)) {
    | None => true
    | Some(_) => false
    }
  | Many => true
  };
};

let getValuesSuggestions =
    (
      ~definedArgs: list((Args.t, 'a)),
      ~argsMap,
      ~currentArg: (string, array(string)),
    )
    : list((string, string)) => {
  let suggestions = ref([]);
  let (currentArgName, _currentArgValues) = currentArg;
  List.iter(
    ((arg, _): (Args.t, 'a)) =>
      switch (arg.name) {
      | argName when argName == currentArgName =>
        if (showArgNameSuggestions(arg, ~argsMap)) {
          suggestions := [(arg.name, arg.doc), ...suggestions^];
        };
        switch (getChoicesForSuggestions(arg.choices, ~argsMap, ~currentArg)) {
        | None => ()
        | Some(choices) =>
          suggestions := List.map(c => (c, arg.doc), choices) @ suggestions^
        };
      | _ =>
        arg.name == ArgsMap.positionalsKey
          ? () : suggestions := [(arg.name, arg.doc), ...suggestions^]
      },
    definedArgs,
  );

  suggestions^;
};

let getPositionalSuggestions =
    (
      ~choices,
      ~children: option(list((string, string))),
      ~argsMap: ArgsMap.t,
      ~currentValues,
    )
    : list((string, string)) => {
  let suggestions =
    switch (
      getChoicesForSuggestions(
        choices,
        ~argsMap,
        ~currentArg=(ArgsMap.positionalsKey, currentValues),
      )
    ) {
    | None => []
    | Some(valueSuggestions) =>
      module S = Seed.DataStructures.StringSet;
      let currentValues = S.fromArray(currentValues);
      List.filter(v => !S.has(v, currentValues), valueSuggestions)
      |> List.map(c => (c, ""));
    };
  switch (children) {
  | None => suggestions
  // sub commands are already relevant to whatever command is active
  // -> should not be excluded from result even if they are duplicated
  | Some(children) => children @ suggestions
  };
};

let getArgChoices =
    (args: list((Args.t, 'a)), ~key): option(Type.Choices.t(string)) =>
  switch (key) {
  | "" => None
  | _ =>
    switch (
      List.find_opt(
        ((arg, _): (Args.t, 'a)) =>
          arg.name == key || Seed.Option.eq(arg.alias, ~v=key),
        args,
      )
    ) {
    | None => None
    | Some((arg, _)) => arg.choices
    }
  };

let getSuggestions =
    (~args: array(string), ~cmd: Cmd.t('a), ~argsMap: ArgsMap.t)
    : list((string, string)) => {
  // -2 to exclude the --suggestions arg and shell
  let last =
    Seed.Arr.slice(args, ~starti=0, ~endi=-2, ()) |> LastArg.ofArray(_);
  switch (last) {
  | Short(currentArg)
  | Long(currentArg)
  | Dash(currentArg) =>
    getValuesSuggestions(~definedArgs=cmd.args, ~argsMap, ~currentArg)
  | Positionals((key, values)) =>
    getPositionalSuggestions(
      ~choices=getArgChoices(cmd.args, ~key),
      ~argsMap,
      ~currentValues=values,
      ~children=
        Seed.Option.map(
          cmd.children,
          ~fn=children => {
            let kvs = Seed.DataStructures.StringMap.entries(children);
            List.map(
              ((k, v: CmdInternal.t('a))) =>
                (k, Seed.Option.getDefault(v.doc, ~default="")),
              kvs,
            );
          },
        ),
    )
  };
};

let getValue = ((value, _)) => value;
let values = vs => List.map(getValue, vs);

let suggestionsForShell = (shell: Seed.Process.Shell.t, suggestions) => {
  switch (shell) {
  | Bash => List.map(((s, _)) => s, suggestions)
  | Zsh => List.map(((s, d)) => s ++ ":" ++ d, suggestions)
  };
};
