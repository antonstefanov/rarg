/**
{!module:RargInternal.Args} - define your command arguments, including their name, documentation and {!module:RargInternal.Type}.
*/
module Args = Args;

/**
{!module:RargInternal.Type} - predefined type parsers and utilities for building your own.
For example [string] [bool] and etc.
Used to parse the input arguments to usable types.
*/
module Type = Type;

/**
{!module:RargInternal.Cmd} - define your commands, including what arguments and sub commands tree.
*/
module Cmd = {
  include Cmd;

  /**
  Convert validation errors to string
  */
  let validateErrToString = (errors: validateErr('a)): list(string) =>
    List.map(
      ((cmd: t('a), err: Cmd.Err.Config.t)) =>
        <Components.Lines marginBottom=1>
          <Components.Line>
            <Components.Span bold=true> {cmd.name} </Components.Span>
          </Components.Line>
          <Components.Line>
            {switch (err) {
             | DuplicateArgs(duplicates) =>
               <ComponentsErrors.DuplicateArgs duplicates />
             | InvalidArgNames(invalidArgs) =>
               <ComponentsErrors.InvalidArgNames args=invalidArgs />
             }}
          </Components.Line>
        </Components.Lines>,
      errors,
    );
};

/**
{!module:RargInternal.Run} - run your commands
*/
module Run = Run;
