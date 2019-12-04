/**
 * Get the closest dir that contains a filename
 */
let rec closestDir = (~dir, ~filename): option(string) =>
  if (Sys.file_exists(Filename.concat(dir, filename))) {
    Some(dir);
  } else {
    switch (Filename.dirname(dir)) {
    // root reached
    | parentDir when parentDir == dir => None
    | parentDir => closestDir(~dir=parentDir, ~filename)
    };
  };
