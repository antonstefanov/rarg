type t = {
  appName: string,
  appPath: string,
  args: array(string),
  argsMap: ArgsMap.t,
};
let getExeAndArgs = (argv: array(string)): (string, array(string)) => (
  argv[0],
  Seed.Arr.slice(argv, ~starti=1, ()),
);
let getBasename = (path: string): string => Filename.basename(path);
let removeExtension = (filename: string) =>
  try (Filename.chop_extension(filename)) {
  | _ => filename
  };
let make = (argv: array(string)): t => {
  let (appPath, args) = getExeAndArgs(argv);
  {
    args,
    appPath,
    appName:
      appPath
      |> getBasename(_)
      |> removeExtension(_)
      |> String.lowercase_ascii(_),
    argsMap: ArgsMap.ofArray(args),
  };
};
