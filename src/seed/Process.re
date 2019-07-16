let isZshShell = () =>
  switch (Sys.getenv_opt("SHELL")) {
  | Some(v) => Strings.contains(v, "zsh")
  | None => false
  };
