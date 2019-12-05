module T = Rarg.Type;
module Cmd = Rarg.Cmd;
module Args = Rarg.Args;

module CmdFruits = {
  type fruit =
    | Apple
    | Banana;
  let fruit: T.t(fruit) = {
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

  let handle = (~fruits: list(fruit), ~bake: bool, ~cut: bool) => ();
  let run = m =>
    handle(~fruits=getFruits(m), ~bake=getBake(m), ~cut=getCut(m));
  let cmd: Cmd.t(unit) =
    Cmd.make(~name="Fruits", ~version="1.1", ~args, ~run, ());
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
          // suggestions are called before input validation -> getCar is not safe to call
          | exception e => []
          },
      ),
    );
  let (args, getModel) =
    Args.One.opt(~args, ~name="--model", ~doc="The car model", model);

  let handle =
      (~car: string, ~cat: string, ~color: string, ~model: option(string)) =>
    ();
  let run = map =>
    handle(
      ~car=getCar(map),
      ~cat=getCat(map),
      ~color=getColor(map),
      ~model=getModel(map),
    );
  let cmd: Cmd.t(unit) =
    Cmd.make(
      ~name="Build a car",
      ~version="1.2",
      ~doc=
        "Allows you to build amazing "
        ++ <Pastel color=Cyan> "cars" </Pastel>
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
  let (args, getBranch) =
    Args.One.opt(~args, ~name="--branch", ~doc="Branch to find", T.branch);

  let handle =
      (~patterns: list(string), ~branch: option(string), ~copy: bool) => {
    print_endline("______________________");
    print_endline("patterns");
    print_endline(patterns |> String.concat(" | ", _));
    print_endline("______________________");
    print_endline("branch");
    print_endline(branch |> Seed.Option.getDefault(_, ~default="__None__"));
    print_endline("______________________");
    print_endline("copy");
    print_endline(copy |> string_of_bool);
    print_endline("______________________");
  };
  let run = m =>
    handle(
      ~patterns=getPatterns(m),
      ~branch=getBranch(m),
      ~copy=getCopy(m),
    );

  let cmd: Cmd.t(unit) =
    Cmd.make(~name="Find", ~version="1.3", ~args, ~run, ());
};

module CmdStart = {
  let args = [];
  let run = m =>
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
