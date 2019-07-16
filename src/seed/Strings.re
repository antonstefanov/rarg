let slice = (str: string, ~starti: int, ~endi: option(int)=?, ()): string => {
  let endIndex =
    switch (endi) {
    | Some(i) => i
    | None => String.length(str)
    };
  String.sub(str, starti, endIndex - starti);
};

let repeat = (str: string, ~times: int): string => {
  let buff = Buffer.create(String.length(str) * times);
  for (_x in 1 to times) {
    Buffer.add_string(buff, str);
  };
  Buffer.contents(buff);
};

let splitAtIndex = (str: string, ~index: int): (string, string) => {
  let left = slice(str, ~starti=0, ~endi=index, ());
  let right = slice(str, ~starti=index + 1, ());
  (left, right);
};
let splitAtChar = (str: string, ~char: char): option((string, string)) => {
  switch (String.index_opt(str, char)) {
  | None => None
  | Some(charIndex) => Some(splitAtIndex(str, ~index=charIndex))
  };
};

let split = (str: string): list(char) => {
  let rec aux = (list, n) => n >= 0 ? aux([str.[n], ...list], n - 1) : list;
  aux([], String.length(str) - 1);
};

let contains = (s1: string, s2: string): bool => {
  let re = Str.regexp_string(s2);
  try (
    {
      ignore(Str.search_forward(re, s1, 0));
      true;
    }
  ) {
  | Not_found => false
  };
};

let startsWith = (str, ~start): bool => {
  switch (String.length(str), String.length(start)) {
  | (fullLen, len) when len > fullLen => false
  | (_, len) =>
    let rec aux = (
      fun
      | i when i == len => true
      | i when str.[i] != start.[i] => false
      | i => aux(i + 1)
    );
    aux(0);
  };
};
