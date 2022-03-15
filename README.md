# Applied Functional Programming Course

[![Build Status](https://travis-ci.org/qfpl/applied-fp-course.svg?branch=master)](https://travis-ci.org/qfpl/applied-fp-course)

![CSIRO's Data61 Logo](https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png)

This is a new course, so there are going to be rough edges. We invite you to
submit issues or pull requests if you find errors or have suggestions on how to
improve it.

This course is designed to be run in a class room with instructors, but we
would like to make it suitable for self-study as well. Although undertaking
this course outside of the workshops will increase the difficulty somewhat,
we do not discourage it and invite suggestions on how to make the course more
approachable.

If you do attempt this on your own and find yourself completely lost,
then you may come find us on IRC on [libera.chat](https://web.libera.chat/)
in `#qfpl` or `#fp-course`.

### You:

* Have completed, or are capable of completing, the [FP Course](https://github.com/system-f/fp-course).
* Have a few months self-study to your name.
* Want to know how to build larger applications with statically typed FP.
* Are willing to accept that a web application is a sufficient choice.
* Can write the canonical function of type: `Applicative f => [f a] -> f [a]`

### We:

* Have constructed a sequence of goals of increasing difficulty.
* Have provided a framework within which to apply these goals.
* Have included relevant components of larger applications:
  * Package dependencies
  * Project configuration
  * Application testing & building
  * Encoding / Decoding messages (JSON & Binary)
  * Persistent storage integration
  * App state & configuration management
  * Error handling & reporting
* Will utilise both type & test driven development techniques.
* Will explain architectural and design trade-offs when appropriate.

### Setup build tools:

Each level is a self-contained Haskell module, containing incomplete, or as yet
undefined, data types and functions.

We recommend using `nix develop` or `nix-shell` to get a shell with
the necessary tools. You can also use `cabal` or `stack`.

#### Cabal

If you're using version >=3.0 of `cabal-install` (use `cabal
--version` to find out), then you don't need to previx the commands
with `v2-`:

```bash
$ cd path/to/applied-fp-course
$ cabal configure --enable-tests
$ cabal build <levelN>-exe
$ $EDITOR src/<LevelN>/README.md
```

#### Nix

If you would like to use a Nix Shell:

```bash
$ cd path/to/applied-fp-course
$ nix-shell  # or `nix develop`, if you use flakes
$ cabal build <levelN>-exe
$ $EDITOR src/<LevelN>/README.md
```

#### ghcid

If you have `ghcid` installed, run `ghcid -c 'cabal repl'` to get a
terminal with up-to-date type errors, which are refreshed on every
save.

#### Stack

A `stack.yaml` configuration is provided on a best-effort basis. The
authors do not use stack and cannot promise to be able to resolve
stack-related issues. Though we will do our best. :)

#### Please note...

These lessons are designed to be completed with an instructor as part of the
Data61 Applied Functional Programming Course. You are of course welcome to
clone the repository and give it a try, but you may find the tasks more
difficult. If you have any questions we can be contacted in the
[#qfpl IRC channel](https://libera.chat) on `libera.chat`.

#### Subsequent lessons may contain spoilers, don't cheat yourself out of the experience!

There is a `README.md` file in each Level module folder that will provide
instructions about what the goal is for that specific level.

* Level 01 : Simple hello world web app.
* Level 02 : Define our application spec with types!
* Level 03 : Testing & tools
* Level 04 : Database layer (sqlite-simple)
* Level 05 : Better error handling through ExceptT
* Level 06 : Add some flexible configuration
* Level 07 : `ReaderT` & refactoring

-- In Development...
* Level 08 : Lenses & "classy mtl" monad transformers

-- Maybe...
* Level 09 : Add session controls (login, logout) and a protected route. So we
  can have something that resembles application state. For the purposes of
  modelling the state machine and implementing some property based tests.

### Bonus Content

Extension material that doesn't feel like it belongs to the main progression
lives in the [`bonus`](https://github.com/qfpl/applied-fp-course/tree/master/bonus)
subdirectory.
