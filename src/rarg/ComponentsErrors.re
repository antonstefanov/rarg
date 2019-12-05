open Components;

module DidYouMean = {
  let createElement =
      (~items: list(string), ~label="Did you mean:", ~children as _, ()) =>
    <Lines>
      <Line> label </Line>
      <Line indent=2>
        <Span color=Cyan> {String.concat(", ", items)} </Span>
      </Line>
    </Lines>;
};
module MaybeDidYouMean = {
  let createElement =
      (~items: list(string), ~label="Did you mean:", ~children as _, ()) =>
    switch (items) {
    | [] => ""
    | items => <DidYouMean items label />
    };
};

module ValidationError = {
  module ValidationTransform = {
    let createElement = (~arg: Args.t, ~actual: string, ~children as _, ()) => {
      <Span>
        <Span color=White>
          {ComponentsHelp.getArgLabel(arg.name, arg.alias)}
        </Span>
        <Span color=Green>
          " expected type "
          <Span bold=true> {arg.type_} </Span>
        </Span>
        <Span color=Red>
          ", but received"
          <Span bold=true> actual </Span>
        </Span>
      </Span>;
    };
  };
  module ValidationArgsCount = {
    let createElement =
        (
          ~arg: Args.t,
          ~actual: int,
          ~expected: ValidateArgs.Err.expected,
          ~children as _,
          (),
        ) => {
      <Span>
        <Span color=White>
          {ComponentsHelp.getArgLabel(arg.name, arg.alias)}
        </Span>
        <Span color=Green>
          " expected: "
          <Span bold=true>
            {ValidateArgs.Err.stringOfExpected(expected)}
          </Span>
        </Span>
        <Span color=Red>
          " received: "
          <Span bold=true> {string_of_int(actual)} </Span>
        </Span>
        <Span reset=true> {actual == 1 ? " value" : " values"} </Span>
      </Span>;
    };
  };
  let createElement =
      (
        ~arg: Args.t,
        ~validateErr: ValidateArgs.Err.t,
        ~argsMap: ArgsMap.t,
        ~children as _,
        (),
      ) =>
    switch (validateErr) {
    | ArgsCount(({actual, expected}: ValidateArgs.Err.argsCount)) =>
      <ValidationArgsCount arg actual expected />
    | Transform(({actual, info}: ValidateArgs.Err.transform)) =>
      let recommendations = {
        Recommendations.forArgValues(
          arg,
          ~argsMap,
          ~currentArgKey=actual,
          ~currentArgValues=[||],
        )
        |> Suggestions.values(_);
      };
      <Lines>
        <Line>
          {switch (info) {
           | Some(info) => info
           | None => <ValidationTransform arg actual />
           }}
        </Line>
        <MaybeDidYouMean items=recommendations />
      </Lines>;
    };
};

module ValidationErrors = {
  let createElement =
      (~validationErrors, ~cmd, ~appName, ~argsMap, ~children as _, ()) =>
    <Lines>
      ...{List.map(
        ((arg, validateErr)) =>
          <Lines>
            <Line> <ValidationError arg validateErr argsMap /> </Line>
            {renderIfTrue(arg.name == ArgsMap.positionalsKey, () =>
               <Line> <ComponentsHelp.CommandSignature cmd appName /> </Line>
             )}
          </Lines>,
        validationErrors,
      )}
    </Lines>;
};

module DuplicateArgs = {
  let createElement = (~duplicates, ~children as _, ()) => {
    <Lines>
      <Line marginBottom=1>
        <Span> "You must define unique argument names. " </Span>
        <Span bold=true> "Duplicates:" </Span>
      </Line>
      <Line>
        ...{List.map(
          ((key, d1: Args.t, d2: Args.t)) =>
            <Lines marginBottom=2>
              <Line> <Span color=Red> key </Span> </Line>
              <Line> <ComponentsHelp.ArgsSignatures args=[d1, d2] /> </Line>
            </Lines>,
          duplicates,
        )}
      </Line>
    </Lines>;
  };
};

module InvalidArgNames = {
  let createElement = (~args, ~children as _, ()) => {
    <Lines>
      <Line marginBottom=1>
        <Span> "Argument names must start with a -" </Span>
      </Line>
      <Line>
        ...{List.map(
          (arg: Args.t) =>
            <Lines marginBottom=2>
              <Line> <ComponentsHelp.ArgsSignatures args=[arg] /> </Line>
            </Lines>,
          args,
        )}
      </Line>
    </Lines>;
  };
};
module UnknownArgs = {
  module UnknownPositionals = {
    exception
      PositionalArgCannotBeUnknown(option(Args.t), option(list(string)));
    let createElement =
        (
          ~unknownPositionals,
          ~definedPositionals,
          ~childCommands,
          ~children as _,
          (),
        ) => {
      switch (unknownPositionals) {
      | None => ""
      | Some(argValues) =>
        let argValuesString =
          Array.to_list(argValues) |> String.concat(" ", _);
        switch (definedPositionals, childCommands) {
        | (None, None) =>
          <Lines marginBottom=1>
            <Line> "Did not expect positional arguments, but received:" </Line>
            <Line indent=2> <Span color=Red> argValuesString </Span> </Line>
          </Lines>
        | (None, Some(children)) =>
          let argValues = Array.to_list(argValues);
          let firstPositional = List.hd(argValues);
          let childrenRecommendations =
            Recommendations.get(
              firstPositional,
              ~candidates=children,
              ~getValue=v => v,
              ~compare=String.compare,
              ~threshold=999,
              (),
            );
          <Lines marginBottom=1>
            <Line> "Unknown sub command:" </Line>
            <Line indent=2> <Span color=Red> argValuesString </Span> </Line>
            <MaybeDidYouMean
              label="Did you mean to call:"
              items=childrenRecommendations
            />
          </Lines>;
        | (_, _) =>
          raise(
            PositionalArgCannotBeUnknown(definedPositionals, childCommands),
          )
        };
      };
    };
  };
  let createElement =
      (
        ~unknownArgs: list((string, array(string))),
        ~definedArgs: list((Args.t, 'a)),
        ~childCommands: option(list(string)),
        ~children as _,
        (),
      ) => {
    let (definedPositionals, definedOptions) =
      Args.splitPositionalsAndOptionArgs(definedArgs);
    let (unknownPositionals, unknownOptions) =
      List.fold_left(
        (
          (
            pos: option(array(string)),
            options: list((string, array(string))),
          ),
          (argKey, argValues),
        ) =>
          argKey === ArgsMap.positionalsKey
            ? (Some(argValues), options)
            : (pos, [(argKey, argValues), ...options]),
        (None, []),
        unknownArgs,
      );
    let namesAndAliases =
      List.fold_left(
        (acc, arg: Args.t) =>
          switch (arg.alias) {
          | None => [arg.name, ...acc]
          | Some(alias) => [arg.name, alias, ...acc]
          },
        [],
        definedOptions,
      );
    <Lines>
      <UnknownPositionals
        unknownPositionals
        definedPositionals
        childCommands
      />
      <Maybe cond={Seed.Lst.hasElements(unknownOptions)}>
        <Lines>
          <Line> "Unknown arguments provided:" </Line>
          <Line>
            ...{List.map(
              ((argKey, argValues)) => {
                let argValuesString =
                  Array.to_list(argValues) |> String.concat(" ", _);
                let recommendations =
                  Recommendations.get(
                    argKey,
                    ~candidates=namesAndAliases,
                    ~getValue=v => v,
                    ~compare=String.compare,
                    (),
                  );
                <Lines marginBottom=2>
                  <Line indent=2>
                    <Span color=Red> argKey </Span>
                    " "
                    argValuesString
                  </Line>
                  <MaybeDidYouMean items=recommendations />
                </Lines>;
              },
              unknownOptions,
            )}
          </Line>
        </Lines>
      </Maybe>
    </Lines>;
  };
};
