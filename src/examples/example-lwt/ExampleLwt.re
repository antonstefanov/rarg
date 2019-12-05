/**
You can define a logo to be displayed in --help
*/
let logo =
  <Pastel color=Green>
    {|
██████╗  █████╗ ██████╗  ██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝
██████╔╝███████║██████╔╝██║  ███╗
██╔══██╗██╔══██║██╔══██╗██║   ██║
██║  ██║██║  ██║██║  ██║╚██████╔╝
╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝
|}
  </Pastel>;
// esy x example-lwt --help
let main =
  Lwt_main.run(
    {
      switch (Rarg.Run.autorun(~logo, LwtCmds.ExampleLwtCmds.CmdStart.cmd)) {
      | Error(e) =>
        switch (e) {
        | ConfigError => exit(100)
        | UserError => exit(10)
        | UnknownError => exit(1)
        }
      | Ok(ok) =>
        switch (ok) {
        | Handled => Lwt.return()
        | Run(promise) =>
          let%lwt result = promise;
          switch (result) {
          | Ok(ok) =>
            switch (ok) {
            // normally there shouldn't be a need to return ok or error variants
            // since these outcomes can be handled directly in the cmd handler
            // however for illustration purposes - you can propagate to the top any error or success
            | FoundFiles => Lwt_io.printl("ExampleLwt.FoundFiles")
            | CutFruits(fruits) =>
              let%lwt () = Lwt_io.printl("Successfully cut fruits:");
              fruits
              |> List.map(LwtCmds.ExampleLwtCmds.CmdCut.fruit.stringify)
              |> String.concat(", ", _)
              |> Lwt_io.printl(_);
            }
          | Error(e) =>
            let%lwt () = Lwt_io.printl(e);
            exit(1);
          };
        }
      };
    },
  );
