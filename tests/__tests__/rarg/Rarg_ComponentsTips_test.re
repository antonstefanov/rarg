open TestFramework;
open RargInternal.ComponentsTips;

describe("Rarg_ComponentsTips", t => {
  t.describe("AddPathTip", t => {
    t.test("shows a tip for nix bash", t =>
      t.expect.string(<AddPathTip appPath="/a/b/c" zsh=false platform=Unix />).
        toMatchSnapshot()
    );
    t.test("shows a tip for nix zsh", t =>
      t.expect.string(<AddPathTip appPath="/a/b/c" zsh=true platform=Unix />).
        toMatchSnapshot()
    );
    t.test("shows a tip for mac bash", t =>
      t.expect.string(
        <AddPathTip appPath="/a/b/c" zsh=false platform=Darwin />,
      ).
        toMatchSnapshot()
    );
    t.test("shows a tip for mac zsh", t =>
      t.expect.string(
        <AddPathTip appPath="/a/b/c" zsh=true platform=Darwin />,
      ).
        toMatchSnapshot()
    );
  });
  t.describe("InstallScript", t => {
    t.test("renders install script for bash", t =>
      t.expect.string(
        <InstallScript appName="my-app" appPath="/a/b/c" zsh=false />,
      ).
        toMatchSnapshot()
    );
    t.test("renders install script for zsh", t =>
      t.expect.string(
        <InstallScript appName="my-app" appPath="/a/b/c" zsh=true />,
      ).
        toMatchSnapshot()
    );
  });
});
