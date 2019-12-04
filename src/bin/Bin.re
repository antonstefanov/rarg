open Ds.Components;

let () =
  Lwt_main.run(
    {
      let%lwt () = Lwt_io.printl("Starting...");
      let two_seconds = Lwt_unix.sleep(2.);
      let one_second = Lwt_unix.sleep(1.);

      let%lwt () = Lwt.join([two_seconds, one_second]);
      Lwt_io.printl(<Span color=Magenta> "Hello World!" </Span>);
    },
  );
