open Components;

module AddPathTip = {
  let installTip =
      (
        ~appPath,
        ~zsh: option(bool)=?,
        ~platform: option(Seed.Os.Platform.t)=?,
        (),
      ) => {
    let isZsh =
      Seed.Option.getDefault(zsh, ~default=Seed.Process.isZshShell());
    let currentPlatform =
      Seed.Option.getDefault(platform, ~default=Seed.Os.Platform.current());
    let bashLocation =
      isZsh
        ? Seed.Os.Zshrc.location(currentPlatform)
        : Seed.Os.Bashrc.location(currentPlatform);
    // {{app_path}} --rarg-add-path >> {{bashrc_location}}
    String.concat(" ", [appPath, ArgsMap.addPathKey, ">>", bashLocation]);
  };
  let createElement =
      (
        ~appPath,
        ~zsh: option(bool)=?,
        ~platform: option(Seed.Os.Platform.t)=?,
        ~children as _,
        (),
      ) => {
    let tip = installTip(~appPath, ~zsh?, ~platform?, ());
    <Lines>
      <Line>
        <Span color=Cyan> "To append to your bash config run:" </Span>
      </Line>
      <Line> <Span color=White> tip </Span> </Line>
      <Line>
        <Span color=Cyan> "Afterwards restart your terminal" </Span>
      </Line>
    </Lines>;
  };
};

module InstallScript = {
  let createElement =
      (~appName, ~appPath, ~zsh: option(bool)=?, ~children as _, ()) => {
    let appAlias =
      String.concat("", ["alias ", appName, "=", "'", appPath, "'"]);

    <Lines>
      <Line> "# " appName </Line>
      <Line> appAlias </Line>
      <Line> "" </Line>
      <Line>
        {TerminalTemplates.Autocomplete.replace(~appName, ~appPath, ~zsh?, ())}
      </Line>
    </Lines>;
  };
};
