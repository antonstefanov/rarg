# ![rarg logo](./rarg.png)

A simple, focused and expressive library for building command line applications.
Optimised for native ReasonML/OCaml.

[![Build Status](https://antonstefanov.visualstudio.com/rarg/_apis/build/status/antonstefanov.rarg?branchName=master)](https://antonstefanov.visualstudio.com/rarg/_build/latest?definitionId=2&branchName=master)

## Features

- **sub commands** - you can easily define a whole tree of commands
- **autocompletion** - fast and comprehensive autocompletion of commands, arguments and values
- **auto configuration validation** - you can validate your whole sub commands tree configuration with a single function call in your tests
- **auto help generation**
- **autocorrection**

## [API Reference](https://rarg.z13.web.core.windows.net/rarg/Rarg/index.html)

> Note that the only external modules are:
>
> - [Type](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Type/index.html)
> - [Args](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Args/index.html)
> - [Cmd](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Cmd/index.html)
> - [Run](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Run/index.html)

## Usage

0. To add to your [esy](https://esy.sh) project simply use:

```sh
esy add rarg
```

1. Define command arguments with [Args](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Args/index.html).

```reasonml
module MyCmd = {
  let args = []
  let (args, getCopy) = Args.One.flag(
      ~args,
      ~name="--copy",
      ~doc="Whether to copy",
      ~default=false,
      Type.bool,
    );
  let (args, getColor) =
    Args.One.default(
      ~args,
      ~name="--color",
      ~doc="Paint color",
      ~default="green",
      Type.string,
    );

  ...
```

For the [Type](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Type/index.html) argument you can either choose one of the [predefined argument types](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Type/index.html#type_predefined) or define custom argument types, for example:

```reasonml
  ...

  type fruit = | Apple | Banana;
  let fruit: Type.t(fruit) = {
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
  let (args, getFruits) = Args.Many.req(~args, ~name="--fruits", ~doc="Fruits", fruit);

  ...
```

2. Define the command with [Cmd](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Cmd/index.html):

```reasonml
  ...

  let handle = (~fruits: list(fruit), ~copy: bool, ~color: string) => ();
  // Define a run function that will use the getters returned from `Args` to get all provided user arguments.
  let run = m => handle(~fruits=getFruits(m), ~copy=getCopy(m), ~color=getColor(m));
  let cmd: Cmd.t(unit) = Cmd.make(~name="My Command", ~args, ~run, ());
} // module MyCmd close
```

You can also easily define sub commands:

```reasonml
module AnotherCmd = {
  ...

  let cmd: Cmd.t(unit) =
    Cmd.make(
      ~name="Another Command",
      ~args,
      ~run,
      ~children=[("my-cmd", MyCmd.cmd)],
      (),
    );
}
```

3. And finally you can run your command with [Run](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Run/index.html)

```reasonml
let main = {
  switch (Run.autorun(MyCmd.cmd)) {
  | Ok(_) => exit(0)
  | Error(_) => exit(1)
  };
};
```

## System arguments (auto-included)

- `--help` - display command help
- `--rarg-suggestions-script` - displays a script with instsructions how to install it to enable autocompletion
- `--rarg-add-path` - displays a script with instructions how to add the app executable to the user's path

## Examples

You can check the local [examples](https://github.com/antonstefanov/rarg/tree/master/src/examples) or the repo [rarg-examples](https://github.com/antonstefanov/rarg-examples) for more complete examples.

## Design decisions

(work in progress)
`rarg`

- does not throw exceptions. It follows the philosophy that if you can predict it, then it's not an exception, so instead `rarg` uses `result(ok, err)` extensively
- does not execute your `run` function. It returns an action to be taken and it's your responsibility to execute the function with all validated arguments. This makes it trivial to
  - work with sync and async code
  - return any exit codes
- is not `POSIX` and `GNU` compliant (and does not aim to be), if you are looking for a compliant native library - check out [cmdliner](https://github.com/dbuenzli/cmdliner)

## How it works

(work in progress)
`rarg`

- parses the user input in a raw form - key and array of values
- combines the parsed input with the provided commands tree (`Cmd.t`)
- finds which command from the tree was requested (it follows any sub-commands)
- determines what action to take
- handles the actions it can (check [Run.re](https://github.com/antonstefanov/rarg/blob/master/src/rarg/Run.re))
- returns back control to the user code

```
+----------+
|          |
| Commands |                  getRunAction
| tree     |
|          |   +---------+   +-----------+   +--------+   +---------+
+------------->+         |   |           |   |        |   |         |
               | Find    |   | Determine |   | Handle |   | Return  |
+------------->+ command +-->+ action    +-->+ action +-->+ control |
|          |   |         |   |           |   |        |   |         |
| User     |   +---------+   +-----------+   +--------+   +---------+
| input    |
|          |
+----------+                  Possible actions
                              Ok: Run, Help, Suggest, Autocomplete and etc.
                              Error: ConfigError, UserError and etc.
```

## Notes

All commands must follow the following structure:

```sh
command [..sub-commands] [..positionals] [..options]
```

The main `command`, optionally followed by `sub-commands`, then optional `positionals` and finally `options` (like `--foo`).
Options always come last and cannot be between subcommands and positionals.
This consistent structure allows for more relevant autocomplete functionality.
