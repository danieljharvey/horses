const repl = require("repl");

/*
{ init: A,
  next: A -> string -> [A,string],
  intro: string,
  prompt: string
  }
*/

const createRepl = (mimsaRepl) => {
  let mutableState = mimsaRepl.init;

  function myEval(cmd, _context, _filename, callback) {
    const cleanCmd = cmd.trim();
    const [state, str] = mimsaRepl.next(mutableState)(cleanCmd);
    mutableState = state;
    callback(null, str);
  }
  
  console.log(mimsaRepl.intro);
  repl.start({ prompt: mimsaRepl.prompt, eval: myEval });
};


createRepl(main);
