open Components;

module AddPathTip = {
  let installTip =
      (
        ~appPath,
        ~shell: option(Seed.Process.Shell.t),
        ~platform: option(Seed.Os.Platform.t)=?,
        (),
      ) => {
    let bashLocation =
      Seed.Process.Shell.getConfigLocation(~shell?, ~platform?, ());
    // {{app_path}} --rarg-add-path >> {{bashrc_location}}
    String.concat(" ", [appPath, ArgsMap.addPathKey, ">>", bashLocation]);
  };
  let createElement =
      (
        ~appPath,
        ~shell: option(Seed.Process.Shell.t)=?,
        ~platform: option(Seed.Os.Platform.t)=?,
        ~children as _,
        (),
      ) => {
    let tip = installTip(~appPath, ~shell, ~platform?, ());
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
      (
        ~appName,
        ~appPath,
        ~shell: option(Seed.Process.Shell.t)=?,
        ~children as _,
        (),
      ) => {
    let appAlias =
      String.concat("", ["alias ", appName, "=", "'", appPath, "'"]);

    <Lines>
      <Line> "# " appName </Line>
      <Line> appAlias </Line>
      <Line> "" </Line>
      <Line>
        {TerminalTemplates.Autocomplete.replace(
           ~appName,
           ~appPath,
           ~shell?,
           (),
         )}
      </Line>
    </Lines>;
  };
};
