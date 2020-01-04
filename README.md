# ![rarg logo](./rarg.png)

A simple, focused and expressive library for building command line applications.
Optimised for native ReasonML/OCaml.

[![Build Status](https://antonstefanov.visualstudio.com/rarg/_apis/build/status/antonstefanov.rarg?branchName=master)](https://antonstefanov.visualstudio.com/rarg/_build/latest?definitionId=2&branchName=master)

## Features

- **autocompletion** - fast and comprehensive autocompletion of commands, arguments and values
- **sync** and **async** commands support
- **sub commands** - you can easily define a whole tree of commands
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

```reason
module Args = Rarg.Args;
module Type = Rarg.Type;

module MyCmd = {
  let args = []
  let (args, getCopy) = Args.One.boolFlag(
      ~args,
      ~name="--copy",
      ~doc="Whether to copy",
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

  // ...
};
```

For the [Type](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Type/index.html) argument you can either choose one of the [predefined argument types](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Type/index.html#type_predefined) or define custom argument types, for example:

```reason
  // ...

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

  // ...
```

2. Define the command with [Cmd](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Cmd/index.html):

```reason
  // ...

  // Define the function that you want to execute
  let handle = (~fruits: list(fruit), ~copy: bool, ~color: string) => ();

  // Define a run function that will use the getters returned from `Args` to get all provided user arguments. It allows you to use labeled arguments as opposed to relying on arg positions.
  let run = m => handle(~fruits=getFruits(m), ~copy=getCopy(m), ~color=getColor(m));

  // Define a command record that you can use to run your command,
  // pass it as a child to other commands or test it
  let cmd: Cmd.t(unit) = Cmd.make(~name="My Command", ~version="1.0", ~args, ~run, ());
} // module MyCmd close
```

You can also easily define sub commands:

```reason
module AnotherCmd = {
  // ...

  let cmd: Cmd.t(unit) =
    Cmd.make(
      ~name="Another Command",
      ~version="1.0",
      ~args,
      ~run,
      ~children=[("my-cmd", MyCmd.cmd)],
      (),
    );
};
```

> In `rarg` every command/subcommand is a complete unit of work, that can exist on its own, has no dependencies of its parents. That's why every command has its own version.

3. And finally you can run your command with [Run](https://rarg.z13.web.core.windows.net/rarg/RargInternal/Run/index.html)

```reason
let main = {
  switch (Run.autorun(MyCmd.cmd)) {
  | Ok(_) => exit(0)
  | Error(_) => exit(1)
  };
};
```

## System arguments (auto-included)

- `--help` - display command help
- `--version` - display command version
- `--rarg-suggestions-script` - displays a script with instsructions how to install it to enable shell autocompletions
- `--rarg-add-path` - displays a script with instructions how to add the app executable to the user's path (helpful during development)

## Examples

You can check the local [examples](https://github.com/antonstefanov/rarg/tree/master/src/examples) or the repo [rarg-examples](https://github.com/antonstefanov/rarg-examples) for more complete examples.

## Comparison with [cmdliner](https://github.com/dbuenzli/cmdliner)

> This was the most requested comparison and is added for completeness, but the 2 are very different.

- `cmdliner` is hosted on `opam` | `rarg` on `npm`
- it's likely that you would be more familiar with `cmdliner`'s API if you have an `OCaml` background and with `rarg`'s API if you are coming from other languages (including `JS`)
- `cmdliner` is very mature and has a large ecosystem behind it, if you are already using it there's no point of switching to `rarg`
- `rarg` has autocompletions, smaller API footprint and is simpler and less abstract in nature

## Notes

All commands must follow the following structure:

```sh
command [..sub-commands] [..positionals] [..options]
```

The main `command`, optionally followed by `sub-commands`, then optional `positionals` and finally `options` (like `--foo`).
Options always come last and cannot be between subcommands and positionals.
This consistent structure allows for more relevant autocomplete functionality and predictable options value parsing.
