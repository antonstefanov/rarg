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
    ) => {
  let suggestions = ref([]);
  let (currentArgName, currentArgValues) = currentArg;

  switch (currentArgValues) {
  | [||] =>
    List.iter(
      ((arg, _): (Args.t, 'a)) =>
        switch (arg.name) {
        | argName when argName == currentArgName =>
          if (showArgNameSuggestions(arg, ~argsMap)) {
            suggestions := [arg.name, ...suggestions^];
          };
          switch (
            getChoicesForSuggestions(arg.choices, ~argsMap, ~currentArg)
          ) {
          | None => ()
          | Some(choices) => suggestions := choices @ suggestions^
          };
        | argName when Seed.Strings.startsWith(argName, ~start=currentArgName) =>
          suggestions := [arg.name, ...suggestions^]
        | _ => suggestions := [arg.name, ...suggestions^]
        },
      definedArgs,
    )
  | _ =>
    List.iter(
      ((arg, _): (Args.t, 'a)) =>
        switch (arg.name) {
        | argName when argName == currentArgName =>
          if (showArgNameSuggestions(arg, ~argsMap)) {
            suggestions := [arg.name, ...suggestions^];
          };
          switch (
            getChoicesForSuggestions(arg.choices, ~argsMap, ~currentArg)
          ) {
          | None => ()
          | Some(choices) =>
            // TODO: consider arg.possibleValues
            suggestions := choices @ suggestions^
          };
        | argName when Seed.Strings.startsWith(argName, ~start=currentArgName) =>
          suggestions := [arg.name, ...suggestions^]
        | _ => suggestions := [arg.name, ...suggestions^]
        },
      definedArgs,
    )
  };
  suggestions^;
};

let getPositionalSuggestions =
    (
      ~choices,
      ~children: option(list(string)),
      ~argsMap: ArgsMap.t,
      ~currentValues,
    ) => {
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
      List.filter(v => !S.has(v, currentValues), valueSuggestions);
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
  switch (
    List.find_opt(
      ((arg, _): (Args.t, 'a)) =>
        arg.name == key || Seed.Option.eq(arg.alias, ~v=key),
      args,
    )
  ) {
  | None => None
  | Some((arg, _)) => arg.choices
  };

let getSuggestions =
    (~args: array(string), ~cmd: Cmd.t('a), ~argsMap: ArgsMap.t)
    : list(string) => {
  // -1 to exclude the --suggestions arg
  let last =
    Seed.Arr.slice(args, ~starti=0, ~endi=-1, ()) |> LastArg.ofArray(_);
  switch (last) {
  | Short(currentArg)
  | Long(currentArg)
  | Dash(currentArg) =>
    getValuesSuggestions(~definedArgs=cmd.args, ~argsMap, ~currentArg)
  | Positionals((key, values)) =>
    getPositionalSuggestions(
      ~choices=getArgChoices(cmd.args, ~key),
      ~argsMap,
      ~children=Seed.Option.map(cmd.children, ~fn=CmdInternal.Sub.keys),
      ~currentValues=values,
    )
  };
};
