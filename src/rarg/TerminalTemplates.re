module type TipTemplateConfig = {
  let templateName: string;
  let bashTemplate: string;
  let zshTemplate: string;
};

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

module type PathTemplateConfig = {
  let templateName: string;
  let bashTemplate: string;
  let zshTemplate: string;
};
module PathTemplate = (Templates: PathTemplateConfig) => {
  let replaceAppName = (template, ~appName) =>
    Str.global_replace(Str.regexp("{{app_name}}"), appName, template);
  let replaceAppPath = (template, ~appPath) =>
    Str.global_replace(Str.regexp("{{app_path}}"), appPath, template);

  let beginTemplate = {|###-begin-{{app_name}}-completions-###|};
  let doNotEdit = {|# Autogenerated, do not edit manually
########################################|};
  let endTemplate = {|###-end-{{app_name}}-completions-###|};

  let wrap = template =>
    String.concat("\n", [beginTemplate, doNotEdit, template, endTemplate]);

  let replace =
      (~shell: option(Seed.Process.Shell.t)=?, ~appName, ~appPath, ()) => {
    let template =
      switch (
        Seed.Option.getDefault(
          shell,
          ~default=Seed.Process.Shell.readFromEnv(),
        )
      ) {
      | Bash => Templates.bashTemplate
      | Zsh => Templates.zshTemplate
      };
    wrap(template)
    |> replaceAppName(_, ~appName)
    |> replaceAppPath(_, ~appPath);
  };
};

module Autocomplete =
  PathTemplate({
    let templateName = "completions";
    /**
     Autocompletion templates based on yargs:
     https://github.com/yargs/yargs/blob/master/lib/completion-templates.js
    */
    // "${args[@]:1}" is used to exclude the command name from the list
    let bashTemplate = {|_rarg_{{app_name}}_completions()
{
    local cur_word args type_list

    cur_word="${COMP_WORDS[COMP_CWORD]}"
    args=("${COMP_WORDS[@]}")

    # ask rarg to generate completions.
    type_list=$({{app_path}} "${args[@]:1}" --rarg-suggestions-request bash)

    COMPREPLY=( $(compgen -W "${type_list}" -- ${cur_word}) )

    # if no match was found, fall back to filename completion
    if [ ${#COMPREPLY[@]} -eq 0 ]; then
      COMPREPLY=()
    fi

    return 0
}
complete -o default -F _rarg_{{app_name}}_completions {{app_name}}|};

    let zshTemplate = {|_rarg_{{app_name}}_completions()
{
  local reply
  local si=$IFS
  IFS=$'\n' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" {{app_path}} "${words[@]:1}" --rarg-suggestions-request zsh))
  IFS=$si
  _describe 'values' reply
}
compdef _rarg_{{app_name}}_completions {{app_name}}|};
  });
