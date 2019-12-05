open Components;

module AddPathTip = {
  let installTip =
      (
        ~appPath,
        ~shell: option(Seed.Process.Shell.t),
        ~platform: option(Seed.Os.Platform.t)=?,
        (),
      ) => {
    let shellLocation =
      Seed.Process.Shell.getConfigLocation(~shell?, ~platform?, ());
    // {{app_path}} --rarg-add-path >> {{bashrc_location}}
    String.concat(" ", [appPath, ArgsMap.addPathKey, ">>", shellLocation]);
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
        <Span color=Cyan> "To append to your shell config run:" </Span>
      </Line>
      <Line> <Span color=White> tip </Span> </Line>
      <Line>
        <Span color=Cyan> "Afterwards restart your terminal" </Span>
      </Line>
    </Lines>;
  };
};

module AddPathScript = {
  let createElement = (~appName, ~appPath, ~children as _, ()) => {
    let appAlias =
      String.concat("", ["alias ", appName, "=", "'", appPath, "'"]);

    <Lines> <Line> "# " appName </Line> <Line> appAlias </Line> </Lines>;
  };
};

module AutocompleteTip = {
  let installTip =
      (
        ~appName,
        ~shell: option(Seed.Process.Shell.t),
        ~platform: option(Seed.Os.Platform.t)=?,
        (),
      ) => {
    let shellLocation =
      Seed.Process.Shell.getConfigLocation(~shell?, ~platform?, ());
    // {{app_path}} --rarg-add-path >> {{bashrc_location}}
    String.concat(
      " ",
      [appName, ArgsMap.suggestionsScriptKey, ">>", shellLocation],
    );
  };
  let createElement =
      (
        ~appName,
        ~shell: option(Seed.Process.Shell.t)=?,
        ~platform: option(Seed.Os.Platform.t)=?,
        ~children as _,
        (),
      ) => {
    let tip = installTip(~appName, ~shell, ~platform?, ());
    <Lines>
      <Line>
        <Span color=Cyan> "To append to your shell config run:" </Span>
      </Line>
      <Line> <Span color=White> tip </Span> </Line>
      <Line>
        <Span color=Cyan> "Afterwards restart your terminal" </Span>
      </Line>
    </Lines>;
  };
};

module AutocompleteScript = {
  let createElement =
      (
        ~appName,
        ~appPath,
        ~shell: option(Seed.Process.Shell.t)=?,
        ~children as _,
        (),
      ) => {
    <Line>
      {TerminalTemplates.Autocomplete.replace(~appName, ~appPath, ~shell?, ())}
    </Line>;
  };
};
