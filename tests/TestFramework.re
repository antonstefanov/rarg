let projectDir = Seed.ProjectRoot.get();

RargInternal.Components.Span.setMode(HumanReadable);

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir:
        projectDir
        |> (dir => Filename.concat(dir, "tests"))
        |> (dir => Filename.concat(dir, "__snapshots__")),
      projectDir,
    });
});
