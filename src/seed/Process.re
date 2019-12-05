module Shell = {
  type t =
    | Bash
    | Zsh;

  let isZsh = v => Strings.contains(v, "zsh") ? Some(Zsh) : None;
  let readFromEnv = () => {
    let pid = string_of_int(Unix.getppid());
    let proc = Chan.readOne("ps -p " ++ pid ++ " -ocomm=");
    // currently esy overrides the shell environment with
    // env -i /bin/bash --norc --noprofile
    // and the process env is different in dev and release
    // hence the many checks:
    Option.flatMap(proc, ~fn=isZsh)
    |> Option.getDefaultLazy(_, ~default=() => {
         Option.flatMap(Sys.getenv_opt("SHELL"), ~fn=isZsh)
         |> Option.getDefaultLazy(_, ~default=() => {
              Option.flatMap(Sys.getenv_opt("ZSH_NAME"), ~fn=isZsh)
              |> Option.getDefault(_, ~default=Bash)
            })
       });
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
    switch (Option.getDefaultLazy(shell, ~default=() => readFromEnv())) {
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
