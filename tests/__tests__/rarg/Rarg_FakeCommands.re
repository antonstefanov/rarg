module T = Rarg.Type;
module Cmd = Rarg.Cmd;
module Args = Rarg.Args;

module CmdFruits = {
  type t =
    | Apple
    | Banana;
  let fruit: Rarg.Type.t(t) = {
    name: "fruit",
    parse:
      fun
      | "apple" => Ok(Apple)
      | "banana" => Ok(Banana)
      | x => Error(Some(x ++ " is not a fruit.")),
    stringify:
      fun
      | Apple => "apple"
      | Banana => "banana",
    choices: Some(HelpAndSuggestions([Apple, Banana])),
  };
  let args = [];
  let (args, getFruits) =
    Args.Many.req(~args, ~name="--fruits", ~doc="Fruits to cook", fruit);
  let (args, getBake) =
    Args.One.boolFlag(
      ~args,
      ~name="--bake",
      ~doc="Whether to bake the fruit",
      T.bool,
    );
  let (args, getCut) =
    Args.One.boolFlag(
      ~args,
      ~name="--cut",
      ~doc="Whether to cut the fruit",
      T.bool,
    );
  let handle = (~fruits as _: list(t), ~bake as _: bool, ~cut as _: bool) =>
    ();
  let run = m =>
    handle(~fruits=getFruits(m), ~bake=getBake(m), ~cut=getCut(m));
  let cmd: Cmd.t(unit) =
    Cmd.make(~name="Fruits", ~version="1.0", ~args, ~run, ());
};
module CmdCar = {
  let car = T.withChoices(T.string, T.Choices.Suggestions(["alfa", "bmw"]));
  let args = [];
  let (args, getCar) =
    Args.One.req(~args, ~name="--car", ~alias="-c", ~doc="Car to build", car);
  let (args, getCat) = Args.One.req(~args, ~name="--cat", ~doc="Cat", car);
  let (args, getColor) =
    Args.One.default(
      ~args,
      ~name="--color",
      ~doc="Paint color",
      ~default="green",
      T.string,
    );
  let model =
    T.withChoices(
      T.string,
      T.Choices.Dynamic(
        (argsMap, _) =>
          switch (getCar(argsMap)) {
          | car =>
            switch (car) {
            | "alfa" => ["4C"]
            | "bmw" => ["3-series", "5-series"]
            | _ => []
            }
          // suggestions are called before input validation
          // -> getCar is not safe to call, so we return an empty list
          // when suggestions are requested, but we don't have a valid car yet
          | exception _e => []
          },
      ),
    );
  let (args, getModel) =
    Args.One.opt(~args, ~name="--model", ~doc="The car model", model);
  let handler =
      (
        ~car as _: string,
        ~cat as _: string,
        ~color as _: string,
        ~model as _: option(string),
      ) =>
    ();
  let run = map =>
    handler(
      ~car=getCar(map),
      ~cat=getCat(map),
      ~color=getColor(map),
      ~model=getModel(map),
    );
  let cmd: Cmd.t(unit) =
    Cmd.make(
      ~name="Build a car",
      ~version="1.0",
      ~doc=
        "Allows you to build amazing "
        ++ <RargInternal.Components.Span color=Cyan>
             "cars"
           </RargInternal.Components.Span>
        ++ " and more",
      ~args,
      ~run,
      ~children=[("fruit", CmdFruits.cmd)],
      (),
    );
};
module CmdFind = {
  let args = [];
  let (args, getPatterns) =
    Args.Positional.Many.req(
      ~args,
      ~name="patterns",
      ~doc="Patterns to find",
      T.string,
    );
  let (args, getCopy) =
    Args.One.boolFlag(
      ~args,
      ~name="--copy",
      ~doc="Whether to copy the results",
      T.bool,
    );
  let handle = (~patterns as _: list(string), ~copy as _: bool) => ();
  let run = m => handle(~patterns=getPatterns(m), ~copy=getCopy(m));
  let cmd: Cmd.t(unit) =
    Cmd.make(~name="Find", ~version="1.0", ~args, ~run, ());
};
module CmdStart = {
  let args = [];
  let run = _m =>
    print_endline("Forgot to add a subcommand, enter --help for help.");
  let cmd: Cmd.t(unit) =
    Cmd.make(
      ~name="Start",
      ~version="1.0",
      ~args,
      ~run,
      ~children=[
        ("fruits", CmdFruits.cmd),
        ("car", CmdCar.cmd),
        ("find", CmdFind.cmd),
      ],
      (),
    );
};
