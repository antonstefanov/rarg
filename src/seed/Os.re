module Platform = {
  /*
   From esy: https://github.com/esy/esy/blob/master/esy-lib/System.re
   License: https://github.com/esy/esy/blob/master/LICENSE
   */
  type t =
    | Darwin
    | Linux
    | Cygwin
    | Windows /* mingw msvc */
    | Unix /* all other unix-y systems */
    | Unknown;

  let uname = () => {
    let ic = Unix.open_process_in("uname");
    let uname = input_line(ic);
    let () = close_in(ic);
    switch (String.lowercase_ascii(uname)) {
    | "linux" => Linux
    | "darwin" => Darwin
    | _ => Unix
    };
  };
  let current = () => {
    switch (Sys.os_type) {
    | "Unix" => uname()
    | "Win32" => Windows
    | "Cygwin" => Cygwin
    | _ => Unknown
    };
  };
};

module Bashrc = {
  let location =
    Platform.(
      fun
      | Darwin => "~/.bash_profile"
      | Linux
      | Unix
      | Unknown
      | Cygwin => "~/.bashrc"
      | Windows => failwith("Windows is not supported")
    );
};

module Zshrc = {
  let location =
    Platform.(
      fun
      | Darwin => "~/.zsh_profile"
      | Linux
      | Unix
      | Unknown
      | Cygwin => "~/.zshrc"
      | Windows => failwith("Windows is not supported")
    );
};
