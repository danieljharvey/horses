# mimsa

very small language. pls be gentle.

```bash
stack install
stack exec mimsa
```

you should then see:

```bash
~~~ MIMSA ~~~
:help - this help screen
:info <expr> - get the type of <expr>
:bind <name> = <expr> - binds <expr> to <name> and saves it in the environment
:list - show a list of current bindings in the environment
:quit - give up and leave
<expr> - evaluate <expr>, returning it's simplified form and type
```
