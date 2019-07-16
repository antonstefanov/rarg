type argsMap = ArgsMap.t;
type run('a) = argsMap => 'a;

type t('a) = CmdInternal.t('a);

module StringMap = CmdInternal.StringMap;
module Action = CmdInternal.Action;
module Err = CmdInternal.Err;

type cmd_t('a) = t('a);

/**
An error that results from the validation of a command
*/
type validateErr('a) = list((t('a), Err.Config.t));

/** follows the sub commands tree and returns whether there are any configuration errors */
let validate = (cmd: t('a)): result(unit, validateErr('a)) => {
  let rec aux = (~cmds: list(t('a)), ~acc) => {
    switch (cmds) {
    | [] => acc
    | [cmd, ...restCmds] =>
      let nextAcc =
        switch (CmdInternal.Validate.definedArgs(cmd.args)) {
        | Ok(_) => acc
        | Error(err) => [(cmd, err), ...acc]
        };
      switch (cmd.children) {
      | None => aux(~cmds=restCmds, ~acc=nextAcc)
      | Some(children) =>
        aux(~cmds=restCmds @ StringMap.values(children), ~acc=nextAcc)
      };
    };
  };

  switch (aux(~cmds=[cmd], ~acc=[])) {
  | [] => Ok()
  | errors => Error(errors)
  };
};

let make =
    (
      ~name: string,
      ~doc: option(string)=?,
      ~args: list((Args.t, Args.validate)),
      ~run: CmdInternal.run('a),
      ~children: option(list((StringMap.key, t('a))))=?,
      (),
    )
    : t('a) => {
  name,
  doc,
  args,
  run,
  children: Seed.Option.map(children, CmdInternal.Sub.ofList),
};
