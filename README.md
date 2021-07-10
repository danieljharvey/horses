# mimsa

Mimsa is a small programming language.

It is inspired syntactically by `Elm` and `Haskell`.

It works by saving expressions into a database and referencing them by hashes like `Unison`.

It can be used through a `repl` or it's own web-based editor.

It compiles to readable Javascript.

It aims to become a specialised DSL for small backend services, much like `Elm` is for frontends, however currently it's just a fun way to play with functional programming without installing anything.

Try it at [mimsa.isverymuchmybusiness.com](https://mimsa.isverymuchmybusiness.com/)



### Getting started

#### Scratch

When you open Mimsa, you'll see the `Scratch` screen. This is like a repl where you can try out expressions.

<img width="523" alt="Screenshot 2021-07-10 at 18 44 24" src="https://user-images.githubusercontent.com/4729125/125171982-f96a8980-e1ae-11eb-8231-bb3b2a5d50cb.png">

Let's try making a simple expression:

<img width="522" alt="Screenshot 2021-07-10 at 18 44 41" src="https://user-images.githubusercontent.com/4729125/125171988-02f3f180-e1af-11eb-8202-759d8a9f67ae.png">

Note that now we have some input, the `Evaluate` button has appeared. Let's click it:

<img width="501" alt="Screenshot 2021-07-10 at 18 44 47" src="https://user-images.githubusercontent.com/4729125/125171999-0edfb380-e1af-11eb-854b-f1796e40e894.png">

It's evaluated the expression, `20`, in pink, and the type of the expression, `Int`, in blue. Note we have not had to mention any types - in Mimsa they are all inferred.

#### Exploring expressions

At the bottom of the screen we have a set of names. These are all the expressions bound to names in our project.

<img width="508" alt="Screenshot 2021-07-10 at 18 48 22" src="https://user-images.githubusercontent.com/4729125/125172140-cbd21000-e1af-11eb-9123-b6e13f0ce412.png">

If we click on any of them we can view the code for the expression.

<img width="523" alt="Screenshot 2021-07-10 at 18 49 06" src="https://user-images.githubusercontent.com/4729125/125172165-0176f900-e1b0-11eb-823b-167e62054fd4.png">

We can change the code here, and even press `Update` to save a new expression and bind it to the name. This means any new users of the function will use your new version, but any old versions will still use the old version of the function.

Note the `Compile export` button - this allows us to turn this expression into Javascript and download it. `Export` is the default runtime which just exports the function. There are a few other runtimes which allow Mimsa to do various side effects (more on this stuff soon).

There is more info on the [syntax](https://github.com/danieljharvey/mimsa/blob/trunk/compiler/README.md).

#### Other menu items to try

➕ Add a new binding, ie, make a new expression and then give it a name, so that it can be used in other expressions.

🧷 Add a new datatype that can be used in other expressions. The compiler will also try and create you helper functions for manipulating your datatype and save those in another expression.

🧪 Add a unit test. As all values and expressions in Mimsa are immutable, this test will only be run once, and the output stored and linked to all the expressions it uses.

🔎 Type search - find useful functions by searching the type you are looking for, ala [Hoogle](https://hoogle.haskell.org/).
