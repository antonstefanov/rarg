let main = {
  switch (Rarg.Run.autorun(Cmds.ExampleBasicCmds.CmdStart.cmd)) {
  | Ok(_) => exit(0)
  | Error(_) => exit(1)
  };
};
