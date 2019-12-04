/**
 * Get an environment variable value or None if not found
 */
let getOpt = s =>
  try (Some(Sys.getenv(s))) {
  | Not_found => None
  };
