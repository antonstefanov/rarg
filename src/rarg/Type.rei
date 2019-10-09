/**
For defining suggested values in a parser.
These values appear in autocompletions and the command help.
They are not used for validation which should be part of the parsing.
For example you can define static suggestions: [ Choices.HelpAndSuggestions([Apple, Banana]) ]
*/
/**
Or you can define dynamic autocomplete suggestions that depend on any of the provided arguments so far:
{[
  let carModel =
    withChoices(
      string,
      Dynamic(
        (argsMap, _) =>
          switch (getCar(argsMap)) {
          | car =>
            switch (car) {
            | "alfa" => ["4C"]
            | "bmw" => ["3-series", "5-series"]
            | _ => []
            }
          // suggestions are called before input validation
          // -> getCar is not safe to call, so we return an empty list
          // when suggestions are requested, but we don't have a valid car yet
          | exception _e => []
          },
      ),
    );
]}
 */
module Choices: {
  type argsMap = ArgsMap.t;
  type t('a) =
    | /** The values will appear in both help text and suggestions */
      HelpAndSuggestions(
        list('a),
      )
    | /** The values will appear only in suggestions */
      Suggestions(list('a))
    | /** The values will appear only in suggestions */
      Dynamic(
        (argsMap, (string, array(string))) => list('a),
      );

  let map: (t('a), 'a => 'b) => t('b);
};

/**
The parse result contains (the parsed value, and an optional actionable user text)
*/;
type parse('a) = string => result('a, option(string));

/**
Type record used for parsing types from and to [string]
*/;
type t('a) = {
  /** name of the type that will appear in help */
  name: string,
  /** string to type */
  parse: parse('a),
  /** type to string */
  stringify: 'a => string,
  /** values to be used in autocompletion and help */
  choices: option(Choices.t('a)),
};

/**
Creates a reusable [Type] parser with optional autocomplete suggestions,
that you can pass when adding new {!module:Args}.
For example to create a type-safe fruits variant type:
*/
/**
{[
  type fruit = | Apple | Banana;
  let fruit: Type.t(fruit) = {
    name: "fruit",
    parse:
      fun
      | "apple" => Ok(Apple)
      | "banana" => Ok(Banana)
      | x => Error(Some(x ++ " is not a fruit.")),
    stringify:
      fun
      | Apple => "apple"
      | Banana => "banana",
    choices: Some(HelpAndSuggestions([Apple, Banana])),
  };
]}
*/
let make:
  (
    ~name: string,
    ~parse: parse('a),
    ~stringify: 'a => string,
    ~choices: Choices.t('a)=?,
    unit
  ) =>
  t('a);

/**
Extends an existing type parser with a list of possible choices.
For example you can extend the [string] type to autocomplete the value [origin/master] with:
{[
withChoices(string, Suggestions(["origin/master"]))
]}
 */
let withChoices: (t('a), Choices.t('a)) => t('a);

/**
(alias of {!val:withChoices}) Extends an existing type parser with a list of possible choices
For example a string parser can be extended to suggest the values "apple" and "banana"
 */
let with_choices: (t('a), Choices.t('a)) => t('a);

/**
{2:type_predefined Predefined basic type parsers}
*/
let char: t(char);

let string: t(string);

let int: t(int);

let float: t(float);

/**
[bool] type (parses only [true]/[false], check {!val:flag} for parsing [truthy]/[falsey] values)
*/
let bool: t(bool);

let shell: t(Seed.Process.Shell.t);

/**
Parses [truthy]/[falsey] values like [true]/[false] [on]/[off] [yes]/[no] [y]/[n] and [1]/[0]
*/
let flag: t(bool);
