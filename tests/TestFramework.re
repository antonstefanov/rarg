let projectDir = Seed.ProjectRoot.get();

DsInternal.Components.Span.setMode(HumanReadable);

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
