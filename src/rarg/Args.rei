/**
Arguments definition and validation.

Scenarios used in the examples:
{ul
{- Provide an arg with this name and [no values]: [--foo]}
{- Provide an arg with this name and [exactly 1 value]: [--foo value]}
{- Provide an arg with this name and [more than 1 value]: [--foo value1 value2]}
{- [Don't provide] an arg with this name: []}
}
*/

type kind =
  | /** Required argument */
    Req
  | /** Optional argument */
    Opt
  | /** Optional argument with default value */
    OptDefault(string);

type possibleValues =
  | /** 0 or 1 value */
    ZeroOrOne
  | /** 1 value */
    One
  | /** 0 or more values */
    Many;

/**
An argument definition. It contains everything needed for displaying comprehensive help,
argument signature and automatic autocompletion.
*/
type t = {
  /** main name of the argument, for example [--foo] */
  name: string,
  /** alternative name of the argument, for example [-f] */
  alias: option(string),
  /** description to show in the help */
  doc: string,
  /** for example [bool] [string] [file] and etc. (shown in the help) */
  type_: string,
  /** required values count */
  possibleValues,
  /** whether the argument is required or not */
  kind,
  /** possible values for the argument, used only in help and suggestions, not for validation */
  choices: option(Type.Choices.t(string)),
};

/**
A function for validating an argument (including its {!module:Type}, the count of expected values and etc.)
*/
type validate = ArgsMap.t => result(unit, ValidateArgs.Err.t);

/** A tuple that contains: (a list of arguments, a value getter) */
type argValidateTuple('a) = (list((t, validate)), ArgsMap.t => 'a);

/**
Separates the positionals and options from an arguments list
*/
let splitPositionalsAndOptionArgs: list((t, 'a)) => (option(t), list(t));

/**
{2:args_one One expected value}
*/
module One: {
  /**
  0 or 1 value, if users:
{ul
{- Provide an arg with this name and [no values] -> [default] will be returned}
{- Provide an arg with this name and [exactly 1 value] -> [provided value] will be returned}
{- Provide an arg with this name and [more than 1 value] -> [validation error]}
{- [Don't provide] an arg with this name -> [default] will be returned}
}
*/
  let flag:
    (
      ~args: list((t, validate)),
      ~name: string,
      ~alias: string=?,
      ~doc: string,
      ~default: 'a,
      Type.t('a)
    ) =>
    argValidateTuple('a);
  /**
  Required value, if users:
{ul
{- Provide an arg with this name and [no values] -> [validation error]}
{- Provide an arg with this name and [exactly 1 value] -> [provided value] will be returned}
{- Provide an arg with this name and [more than 1 value] -> [validation error]}
{- [Don't provide] an arg with this name -> [validation error]}
}
  */
  let req:
    (
      ~args: list((t, validate)),
      ~name: string,
      ~alias: string=?,
      ~doc: string,
      Type.t('a)
    ) =>
    argValidateTuple('a);
  /**
  Optional with a default value, if users:
{ul
{- Provide an arg with this name and [no values] -> [validation error]}
{- Provide an arg with this name and [exactly 1 value] -> [provided value] will be returned}
{- Provide an arg with this name and [more than 1 value] -> [validation error]}
{- [Don't provide] an arg with this name -> [default] will be returned}
}
  */
  let default:
    (
      ~args: list((t, validate)),
      ~name: string,
      ~alias: string=?,
      ~doc: string,
      ~default: 'a,
      Type.t('a)
    ) =>
    argValidateTuple('a);
  /**
  Optional, if users:
{ul
{- Provide an arg with this name and [no values] -> [validation error]}
{- Provide an arg with this name and [exactly 1 value] -> [provided value] will be returned}
{- Provide an arg with this name and [more than 1 value] -> [validation error]}
{- [Don't provide] an arg with this name -> [None] will be returned}
}
  If you want to allow users to provide an argument, but no values (for example [--copy]), you can check {!val:Args.One.flag}.
  */
  let opt:
    (
      ~args: list((t, validate)),
      ~name: string,
      ~alias: string=?,
      ~doc: string,
      Type.t('a)
    ) =>
    argValidateTuple(option('a));
};

/**
{2:args_one Many expected values}
*/
/** Getters return a [list] that can contain 0 or more elements. */
module Many: {
  /**
  Required, if users:
{ul
{- Provide an arg with this name and [no values] -> [empty list]}
{- Provide an arg with this name and [1 or more values] -> [provided values] will be returned}
{- [Don't provide] an arg with this name -> [validation error]}
}
  */
  let req:
    (
      ~args: list((t, validate)),
      ~name: string,
      ~alias: string=?,
      ~doc: string,
      Type.t('a)
    ) =>
    argValidateTuple(list('a));

  /**
  Optional with a default value, if users:
{ul
{- Provide an arg with this name and [no values] -> [empty list]}
{- Provide an arg with this name and [1 or more values] -> [provided values] will be returned}
{- [Don't provide] an arg with this name -> [default] will be returned}
}
  */
  let default:
    (
      ~args: list((t, validate)),
      ~name: string,
      ~alias: string=?,
      ~doc: string,
      ~default: list('a),
      Type.t('a)
    ) =>
    argValidateTuple(list('a));

  /**
  Optional
{ul
{- Provide an arg with this name and [no values] -> [empty list]}
{- Provide an arg with this name and [1 or more values] -> [provided values] will be returned}
{- [Don't provide] an arg with this name -> [None] will be returned}
}
  */
  let opt:
    (
      ~args: list((t, validate)),
      ~name: string,
      ~alias: string=?,
      ~doc: string,
      Type.t('a)
    ) =>
    argValidateTuple(option(list('a)));
};

/** Positional (anonymous) argument (you can have only one definition for positional argument(s)). */
module Positional: {
  /** One value */
  module One: {
    /** 0 or 1 required values */
    let flag:
      (
        ~args: list((t, validate)),
        ~name: string=?,
        ~doc: string,
        ~default: 'a,
        Type.t('a)
      ) =>
      argValidateTuple('a);

    /** Required */
    let req:
      (
        ~args: list((t, validate)),
        ~name: string=?,
        ~doc: string,
        Type.t('a)
      ) =>
      argValidateTuple('a);

    /** Optional with a default value */
    let default:
      (
        ~args: list((t, validate)),
        ~name: string=?,
        ~doc: string,
        ~default: 'a,
        Type.t('a)
      ) =>
      argValidateTuple('a);

    /** Optional */
    let opt:
      (
        ~args: list((t, validate)),
        ~name: string=?,
        ~doc: string,
        Type.t('a)
      ) =>
      argValidateTuple(option('a));
  };

  /** 0 or more values */
  module Many: {
    /** Required */
    let req:
      (
        ~args: list((t, validate)),
        ~name: string=?,
        ~doc: string,
        Type.t('a)
      ) =>
      argValidateTuple(list('a));

    /** Optional with a default value */
    let default:
      (
        ~args: list((t, validate)),
        ~name: string=?,
        ~doc: string,
        ~default: list('a),
        Type.t('a)
      ) =>
      argValidateTuple(list('a));

    /** Optional */
    let opt:
      (
        ~args: list((t, validate)),
        ~name: string=?,
        ~doc: string,
        Type.t('a)
      ) =>
      argValidateTuple(option(list('a)));
  };
};
