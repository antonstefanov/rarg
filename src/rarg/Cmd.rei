type argsMap = ArgsMap.t;
type run('a) = argsMap => 'a;

type t('a) = CmdInternal.t('a);

module Action = CmdInternal.Action;
module Err = CmdInternal.Err;

/**
An error that results from the validation of a command
*/
type validateErr('a) = list((t('a), Err.Config.t));

/**
Follows the sub commands tree and returns whether there are any configuration errors.
You probably want to call this function with your top level command in your tests.
This would ensure that you don't ship any configuration errors to users.
*/
let validate: t('a) => result(unit, list((t('a), Err.Config.t)));

/**
Create a command.
All commands from a tree need to return the same result from their [run] function.
If you need to differentiate the results from different commands you can define a variant
and use it as a return type.
For example:
{[
type fruit = | Apple | Banana;
type ok = | PeeledFruit(fruit) | CutFruits(list(fruit));
type cmdResult = result(ok, string);
let run = (..): Lwt.t(cmdResult) => ..
]}

You can check the local {{:https://github.com/antonstefanov/rarg/tree/master/src/examples} examples}
or the repo {{:https://github.com/antonstefanov/rarg-examples} rarg-examples} for more complete examples.
*/
let make:
  (
    ~name: string,
    ~version: string,
    ~doc: string=?,
    ~args: list((Args.t, Args.validate)),
    ~run: CmdInternal.run('a),
    ~children: list((string, t('a)))=?,
    unit
  ) =>
  t('a);
