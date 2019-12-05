type t('v, 'e) = result('v, 'e);

exception ExpectedAnOk;
exception ExpectedAnError;

let getOkExn = (result: t('v, 'e)): 'v =>
  switch (result) {
  | Ok(v) => v
  | Error(_) => raise(ExpectedAnOk)
  };

let getErrExn = (result: t('v, 'e)): 'e =>
  switch (result) {
  | Ok(_) => raise(ExpectedAnError)
  | Error(e) => e
  };
