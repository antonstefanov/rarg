open TestFramework;
open RargInternal.ComponentsTips;

describe("Rarg_ComponentsTips", t => {
  t.describe("AddPathTip", t => {
    t.test("shows a tip for nix bash", t =>
      t.expect.string(
        <AddPathTip appPath="/a/b/c" shell=Bash platform=Unix />,
      ).
        toMatchSnapshot()
    );
    t.test("shows a tip for nix zsh", t =>
      t.expect.string(<AddPathTip appPath="/a/b/c" shell=Zsh platform=Unix />).
        toMatchSnapshot()
    );
    t.test("shows a tip for mac bash", t =>
      t.expect.string(
        <AddPathTip appPath="/a/b/c" shell=Bash platform=Darwin />,
      ).
        toMatchSnapshot()
    );
    t.test("shows a tip for mac zsh", t =>
      t.expect.string(
        <AddPathTip appPath="/a/b/c" shell=Zsh platform=Darwin />,
      ).
        toMatchSnapshot()
    );
  });
  t.describe("AddPathScript", t => {
    t.test("renders install script for bash", t =>
      t.expect.string(
        <AddPathScript appName="my-app" appPath="/a/b/c" shell=Bash />,
      ).
        toMatchSnapshot()
    );
    t.test("renders install script for zsh", t =>
      t.expect.string(
        <AddPathScript appName="my-app" appPath="/a/b/c" shell=Zsh />,
      ).
        toMatchSnapshot()
    );
  });
});
