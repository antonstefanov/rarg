module T = Rarg.Type;
module Cmd = Rarg.Cmd;
module Args = Rarg.Args;

type fruit =
  | Apple
  | Banana;
type ok =
  | FoundFiles
  | CutFruits(list(fruit));
type cmdResult = result(ok, string);

module CmdFind = {
  let args = [];
  let (args, getPatterns) =
    Args.Positional.Many.req(
      ~args,
      ~name="patterns",
      ~doc="Patterns to find",
      T.string,
    );
  let handle = (~patterns: list(string)): Lwt.t(cmdResult) => {
    let%lwt () = Lwt_unix.sleep(0.5);
    (
      switch (patterns) {
      | [] => Error("No files provided")
      | [_x] => Ok(FoundFiles)
      | _ => Error("Could not find any files")
      }
    )
    |> Lwt.return;
  };
  let run = m => handle(~patterns=getPatterns(m));
  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(~name="Find", ~args, ~run, ());
};

module CmdCut = {
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
    choices: Some(T.Choices.HelpAndSuggestions([Apple, Banana])),
  };
  let args = [];
  let (args, getFruits) =
    Args.Positional.Many.req(
      ~args,
      ~name="fruits",
      ~doc="Fruits to cut",
      fruit,
    );

  let handle = (~fruits: list(fruit)): Lwt.t(cmdResult) => {
    let%lwt () = Lwt_unix.sleep(0.5);
    Ok(CutFruits(fruits)) |> Lwt.return;
  };
  let run = m => handle(~fruits=getFruits(m));
  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(~name="Cut fruits", ~args, ~run, ());
};

module CmdStart = {
  let args = [];
  let run = _m =>
    Error("Forgot to add a subcommand, enter --help for help.")
    |> Lwt.return(_);
  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(
      ~name="Start",
      ~args,
      ~run,
      ~children=[("find", CmdFind.cmd), ("cut", CmdCut.cmd)],
      (),
    );
};
