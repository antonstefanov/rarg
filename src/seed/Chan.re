let readChannelOnce = (ic): option(string) =>
  try(Some(input_line(ic))) {
  | End_of_file => None
  };

let readChannel = (ic): list(string) => {
  let lines = ref([]);
  try(
    while (true) {
      lines := [input_line(ic), ...lines^];
    }
  ) {
  | End_of_file => ()
  };
  List.rev(lines^);
};

let readOne = (cmd): option(string) => {
  let ic = Unix.open_process_in(cmd);
  let res = readChannelOnce(ic);
  close_in_noerr(ic);
  res;
};

let readMany = (cmd): list(string) => {
  let ic = Unix.open_process_in(cmd);
  let res = readChannel(ic);
  close_in_noerr(ic);
  res;
};

let readPipedInput = () => {
  switch (in_channel_length(stdin)) {
  | 0 => []
  | _ => readChannel(stdin)
  | exception _ => readChannel(stdin)
  };
};
