let slice =
    (arr: array('a), ~starti: int, ~endi: option(int)=?, ()): array('a) => {
  let endIndex =
    switch (endi) {
    | Some(i) when i >= 0 => i
    | Some(i) => Array.length(arr) + i
    | None => Array.length(arr)
    };
  Array.sub(arr, starti, endIndex - starti);
};
