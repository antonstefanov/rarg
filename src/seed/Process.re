module Shell = {
  type t =
    | Bash
    | Zsh;

  let readFromEnv = () => {
    switch (Sys.getenv_opt("SHELL")) {
    | Some(v) => Strings.contains(v, "zsh") ? Zsh : Bash
    | None => Zsh
    };
  };

  module Bashrc = {
    let location =
      Os.Platform.(
        fun
        | Darwin => "~/.bash_profile"
        | Linux
        | Unix
        | Unknown
        | Cygwin => "~/.bashrc"
        | Windows => failwith("Windows is not supported")
      );
  };

  module Zshrc = {
    let location =
      Os.Platform.(
        fun
        | Darwin => "~/.zshrc"
        | Linux
        | Unix
        | Unknown
        | Cygwin => "~/.zshrc"
        | Windows => failwith("Windows is not supported")
      );
  };

  let getConfigLocation = (~shell=?, ~platform=?, ()) => {
    let p = Option.getDefault(platform, ~default=Os.Platform.current());
    switch (Option.getDefault(shell, ~default=readFromEnv())) {
    | Bash => Bashrc.location(p)
    | Zsh => Zshrc.location(p)
    };
  };
};

let isZshShell = () =>
  switch (Sys.getenv_opt("SHELL")) {
  | Some(v) => Strings.contains(v, "zsh")
  | None => false
  };
