/** print an arrow below a string at a certain index */
let arrow = (str: string, ~index: int): unit => {
  let (left, right) = Strings.splitAtIndex(str, ~index);
  let c = String.make(1, str.[index]);
  print_endline(
    <Pastel>
      <Pastel> left <Pastel color=Cyan> c </Pastel> right </Pastel>
    </Pastel>,
  );
  print_endline(
    <Pastel>
      {Strings.repeat(" ", ~times=index)}
      <Pastel color=Cyan> "⤒" </Pastel>
    </Pastel>,
  );
};

/** print 2 arrows above and below a string at left and right indexes */
let arrows = (str: string, ~l: int, ~r: int): unit => {
  print_endline(
    <Pastel>
      {Strings.repeat(" ", ~times=l)}
      <Pastel color={l == r ? Cyan : Green}> "⥙" </Pastel>
    </Pastel>,
  );

  let (left, rest) = Strings.splitAtIndex(str, ~index=l);
  let lchar = String.make(1, str.[l]);
  let content =
    switch (l, r) {
    | (l, r) when l == r =>
      Pastel.(
        <Pastel>
          <Pastel> left <Pastel color=Cyan> lchar </Pastel> rest </Pastel>
        </Pastel>
      )
    | (l, r) =>
      let rchar = String.make(1, str.[r]);
      let (mid, right) = Strings.splitAtIndex(rest, ~index=r - l - 1);
      Pastel.(
        <Pastel>
          left
          <Pastel color=Green> lchar mid rchar </Pastel>
          right
        </Pastel>
      );
    };
  print_endline(content);
  print_endline(
    <Pastel>
      {Strings.repeat(" ", ~times=r)}
      <Pastel color={l == r ? Cyan : Green}> "⥔" </Pastel>
    </Pastel>,
  );
};
