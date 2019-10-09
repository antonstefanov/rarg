type runAction('a) = (
  result(Cmd.Action.t, Cmd.Err.t),
  Cmd.t('a),
  Cmd.argsMap,
);

/**
Follow the sub commands tree and return the action that should be taken
*/
let getRunAction: (~cmd: Cmd.t('a), ~argsMap: Cmd.argsMap) => runAction('a);

/**
A simplified run result. It contains the ready to print strings for help, suggest and etc.
*/
module RunResult: {
  type ok('a) =
    | /** The arguments are valid and the command can be executed */
      Run('a)
    | /** Help was requested */
      Help(string)
    | /** Version was requested */
      Version(string)
    | /** Suggestions for autocompletion were requested */
      Suggest(string)
    | /** A script for autocompletion to be added to the bash config was requested */
      AutoCompleteScript(
        string,
      )
    | /** A script for installation to be added to the bash config was requested */
      AddPath(
        string,
        string,
      )
    | RemovePath;
  type err =
    | /** There was a configuration error, this can be validated before distribution of the application */
      ConfigError(
        string,
      )
    | /** The user has provided some invalid input */
      UserError(string)
    | /** It's unclear if the error is a developer or user mistake */
      UnknownError(
        string,
      );
  type t('a) = result(ok('a), err);
};

/**
Simplifies a run action by returning the concrete strings to be printed for help, suggestions and etc.
*/
let simplify:
  (
    ~runAction: runAction('a),
    ~shell: Seed.Process.Shell.t=?,
    ~platform: Seed.Os.Platform.t=?,
    ~args: array(string),
    ~appName: string,
    ~appPath: string,
    unit
  ) =>
  RunResult.t('a);

/**
The result of an [autorun]
*/
module AutorunResult: {
  type ok('a) =
    | /** The arguments are valid and the command can be executed */
      Run('a)
    | /** The arguments are valid and the action has already been handled */
      Handled;
  type err =
    | /** There was a configuration error, this can be validated before distribution of the application */
      ConfigError
    | /** The user has provided some invalid input */
      UserError
    | /** It's unclear if the error is a developer or user mistake */
      UnknownError;
  type t('a) = result(ok('a), err);
};

/**
Autorun checks the requested action and handles all actions (for example prints help) except [run],
which needs to be handled by the user code.
This makes it trivial to use custom return types, use your own exit codes, or use an async library like [lwt].
*/
let autorun: (~logo: string=?, Cmd.t('a)) => AutorunResult.t('a);
