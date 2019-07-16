open TestFramework;
open RargInternal.ComponentsHelp;

describe("Rarg_ComponentsHelp", t =>
  t.describe("Help", t => {
    let appName = "my-app-name";
    t.test("CmdStart", t =>
      t.expect.string(<Help cmd=Rarg_FakeCommands.CmdStart.cmd appName />).
        toMatchSnapshot()
    );
    t.test("CmdFruits", t =>
      t.expect.string(<Help cmd=Rarg_FakeCommands.CmdFruits.cmd appName />).
        toMatchSnapshot()
    );
    t.test("CmdCar", t =>
      t.expect.string(<Help cmd=Rarg_FakeCommands.CmdCar.cmd appName />).
        toMatchSnapshot()
    );
    t.test("CmdFind", t =>
      t.expect.string(<Help cmd=Rarg_FakeCommands.CmdFind.cmd appName />).
        toMatchSnapshot()
    );
  })
);
