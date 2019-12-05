module S = Seed.DataStructures.StringMap;
open Components;

// TODO(antons): this is quite ugly, for positionals only
// instead of showing <..positionals> show the alias
// revisit how positionals conig is stored,
// there are too many implicit checks with ==ArgsMap.positionalsKey
let getArgLabel = (name, alias) => {
  switch (name == ArgsMap.positionalsKey, alias) {
  | (false, _)
  | (true, None) => name
  | (true, Some(name)) => name
  };
};

module ArgTypeSignature = {
  let createElement = (~arg: Args.t, ~children as _, ()) => {
    let (open_, close) =
      switch (arg.kind) {
      | Req => ("<", ">")
      | Opt => ("[", "]")
      | OptDefault(default) => ("[", "]=" ++ default)
      };
    let labelModifier =
      switch (arg.possibleValues) {
      | ZeroOrOne => ""
      | One => ""
      | Many => ".."
      };

    let choices =
      switch (arg.choices) {
      | None => ""
      | Some(choices) =>
        switch (choices) {
        | HelpAndSuggestions(xs) => " " ++ String.concat(", ", xs)
        | Suggestions(_)
        | Dynamic(_) => ""
        }
      };

    <Span>
      <Span bold=true> open_ labelModifier {arg.type_} close </Span>
      <Span italic=true> choices </Span>
    </Span>;
  };
};

module ArgsSignatures = {
  let argLabel = (name, alias) =>
    switch (name == ArgsMap.positionalsKey, alias) {
    | (false, None) => name
    | (false, Some(alias)) => name ++ " " ++ alias ++ ""
    | (true, None) => name
    | (true, Some(name)) => name
    };
  let createElement = (~args, ~children as _, ()) =>
    List.map(
      (arg: Args.t) =>
        (
          argLabel(arg.name, arg.alias),
          arg.doc ++ " " ++ <ArgTypeSignature arg />,
        ),
      args,
    )
    |> tupleLines(_)
    |> String.concat("\n");
};

module Positionals = {
  let createElement = (~item: option(Args.t), ~children as _, ()) =>
    switch (item) {
    | None => ""
    | Some(arg) =>
      <Lines marginBottom=1>
        <Line> "Positionals:" </Line>
        <ArgsSignatures args=[arg] />
      </Lines>
    };
};

module ArgNameSignature = {
  let createElement =
      (
        ~label: string,
        ~kind: Args.kind,
        ~possibleValues: Args.possibleValues,
        ~children as _,
        (),
      ) => {
    let (open_, close) =
      switch (kind) {
      | Req => ("<", ">")
      | Opt => ("[", "]")
      | OptDefault(default) => ("[", "]=" ++ default)
      };
    let labelModifier =
      switch (possibleValues) {
      | ZeroOrOne => ""
      | One => ""
      | Many => ".."
      };

    <Span> open_ labelModifier label close </Span>;
  };
};

module Options = {
  let createElement = (~items, ~children as _, ()) =>
    switch (items) {
    | [] => ""
    | args =>
      <Lines marginBottom=1>
        <Line> "Options:" </Line>
        <Line> <ArgsSignatures args /> </Line>
      </Lines>
    };
};
module CommandSignatureBase = {
  let createElement =
      (
        ~appName: string,
        ~hasSubCommands: bool,
        ~positionals: option(Args.t),
        ~options: list(Args.t),
        ~children as _,
        (),
      ) => {
    let optionsStr =
      switch (options) {
      | [] => ""
      | [option] =>
        switch (option.kind) {
        | Req => " <options>"
        | Opt
        | OptDefault(_) => " [options]"
        }
      | options =>
        let hasRequired =
          List.exists(
            (o: Args.t) =>
              switch (o.kind) {
              | Req => true
              | Opt => false
              | OptDefault(_) => false
              },
            options,
          );
        hasRequired ? " <..options>" : " [..options]";
      };
    let subCommand =
      hasSubCommands
        ? isSome(positionals) || List.length(options) > 0
            ? " [sub-command]" : " <sub-command>"
        : " ";

    <Span color=White>
      appName
      subCommand
      {renderIfSome(positionals, p =>
         <ArgNameSignature
           kind={p.kind}
           possibleValues={p.possibleValues}
           label={getArgLabel(p.name, p.alias)}
         />
       )}
      optionsStr
    </Span>;
  };
};
module InternalCommandSignature = {
  let createElement =
      (
        ~appName,
        ~hasSubCommands,
        ~positionals: option(Args.t),
        ~options: list(Args.t),
        ~marginBottom=?,
        ~children as _,
        (),
      ) => {
    <Lines ?marginBottom>
      <Line> "Command signature:" </Line>
      <Line indent=2>
        <CommandSignatureBase appName hasSubCommands positionals options />
      </Line>
    </Lines>;
  };
};

module CommandSignature = {
  let createElement = (~cmd: CmdInternal.t('a), ~children as _, ()) => {
    let (positionals, options) =
      Args.splitPositionalsAndOptionArgs(cmd.args);

    <CommandSignatureBase
      hasSubCommands={isSome(cmd.children)}
      positionals
      options
    />;
  };
};
module Commands = {
  let createElement = (~items, ~marginBottom=?, ~children as _, ()) =>
    switch (items) {
    | None => ""
    | Some(children) =>
      let content =
        List.map(
          ((name, cmd): (string, CmdInternal.t('a))) => (name, cmd.name),
          S.entries(children),
        )
        |> tupleLines(_)
        |> String.concat("\n");

      <Lines ?marginBottom>
        <Line> "Sub-Commands:" </Line>
        <Line> content </Line>
      </Lines>;
    };
};

module Help = {
  let createElement =
      (~cmd: CmdInternal.t('a), ~appName: string, ~children as _, ()) => {
    let (positionals, options) =
      Args.splitPositionalsAndOptionArgs(cmd.args);

    <Lines>
      <Line> <Span bold=true> {cmd.name} </Span> </Line>
      {renderIfSome(cmd.doc, doc => <Line marginBottom=1> doc </Line>)}
      <InternalCommandSignature
        appName
        hasSubCommands={isSome(cmd.children)}
        positionals
        options
        marginBottom=1
      />
      <Commands items={cmd.children} />
      <Positionals item=positionals />
      <Options items=options />
    </Lines>;
  };
};
