let get = () => {
  switch (Env.getOpt("RARG_ROOT")) {
  | Some(dir) => dir
  | None =>
    switch (Fs.closestDir(~dir=Sys.getcwd(), ~filename=".rarg-root")) {
    | Some(dir) => dir
    | None =>
      failwith(
        "Expected `REASON_NATIVE_ROOT` environment variable to be set "
        ++ "before running tests.",
      )
    }
  };
};
