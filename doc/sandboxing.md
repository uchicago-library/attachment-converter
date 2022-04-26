# Workflow for using our sandboxed switch

As outlined in [issue
16](https://github.com/uchicago-library/attachment-converter/issues/16),
the Attachment Converter developers are using sandboxed switches,
which we are pleased to say is a feature that `opam` finally offers!
Our starting point for getting all that up and running was [this blog
post](https://khady.info/opam-sandbox.html).

## Overview

The goal is to make our builds reproducible, so that we don't end up
in the proverbial 'why doesn't it work on your machine when it works
on my machine?' situation.  If we're all developing in the same
environment, and a user can recreate our exact dev environment on
their machine in one command, then they will be able to work on our
project (if they want to) without getting stuck in config hell.

`opam` is the package manager for OCaml, and it works by installing
something called _switches_.  A switch, in the `opam` sense of the
term, is a specific compiler version, plus a location to store
compiled code for all OCaml software packages that were installed for
use with that compiler verison.

For our purposes, there are two types of `opam` switches: global and
sandboxed.  When you install a global `opam` switch, say for the
4.12.1 compiler, it is typicaly named `4.12.1`, is located in your
home directory, and contains compiled code for all OCaml software
packages you installed for use with that compiler version.  You can
switch between different switches with the `opam switch` command, and
when you do, you need to update your shell environment variables to
reflect which switch you are now in.  You can do that by typing:

```
$ eval $(opam env)
```

Attachment Converter is intended to be built from the other type of
switch: the sandboxed switch.  When you create that type of switch, it
installs a particular OCaml compiler version along with all your
project's dependencies not to a centralized location in `~/.opam`, as
per the normal custom, but rather in a directory called `_opam` inside
your project itself.  The switch is typically named after the
directory where you project is located, rather than after a specific
compiler version.  Opam will also change into your sandboxed switch
whenever you `cd` into the project directory automatically, and change
back to whatever global switch you were in previously when you `cd`
out.  Note, however, that you may sometimes have to run `eval $(opam
env)` to get your shell variables into whack with the sandboxed switch
after entering the directory.

## The DLDC Opam Repo

The [Digital Library Development
Center](https://uchicago-library.github.io/) finally has an official
`opam` repo, after over a decade of work building up various and
sundry OCaml packages that university libraries can use in their
software applications.  It is available for anyone to start using
here:

https://dldc.lib.uchicago.edu/opam

The focus of the DLDC `opam` repository is on our in-house software
libraries, which at this point mainly means `prelude`, our standard
library for OCaml.  The `opam` repository allows us to easily hook
`opam` up with the GitHub and Mercurial repositories for these
libraries, so that `opam` has access to the latest versions of those
libraries as soon as they are pushed up.

## Workflow

Our workflow for contributions at the level of the sandboxed `opam`
switch breaks up into two parts: creating a new switch, and updating
any changes you've made.

### Creating a New Switch

The information `opam` needs to create a sandboxed switch is contained
in our `attachment-converter.opam.locked` file located at the root of
the project.  This contains all development dependences as well as all
build dependencies.  (Dev-only dependencies are indicated in our
`dune-project` file by the `:dev` symbol.)  It was generated with a
utility called `opam-lock`, which is a separate package from `opam`
but is run as a subcommand of `opam`.

To build Attachment Converter, first create a sandboxed `opam` switch
for it with this command:

```
$ opam switch create . --deps-only --locked --repos dldc='https://dldc.lib.uchicago.edu/opam',default
```

That will create a build environment containing exactly what is needed
to compile Attachment Converter, including `prelude`, and nothing
more.  It will take a few minutes to download and compile all the
necessary packages, so this is a good time to go make a coffee.  Once
the switch is set up, make sure your shell environment is sync-ed up
with it.  If it isn't the `opam switch list` command will give you an
error message telling you to update your shell variables, which you
can do by running `eval $(opam env)`.

### Building

To compile the project, use `make`:

```
$ make
```

To clean up the compiler detritus, run `make clean`:

```
$ make clean
```

To run unit tests, run `make test`:

```
$ make test
```

Finally, to load the code for this project into a project-aware REPL,
use `opam`:

```
$ opam exec -- dune utop
```

You are strongly encouraged to create a new switch and develop in it
whenever you begin work on a new branch.  If all of our developers
follow that practice, it provides some guarantee that we are all
assuming the same dependencies.

### Updating the Sandboxed Switch

Updating our sandboxed switch means updating the `.locked` file.  We
typically won't need to do that, but it is necessary whenever we
modify the project's third-party library dependencies. OCaml's build
toolchain is still pretty complicated, so updating one of our third
party library dependencies involves a couple steps.

The real human-maintained source of information on what our
dependencies are at any given time is the `dune-project` file, located
in the root of the project.  Then we generate the `.opam` file with
`dune`, making the project into a full OCaml package of the form that
`opam` will recognize.  Finally, we run `opam-lock` to compute the
`.locked` file, which provides the complete list of everything `opam`
needs to install to the sandboxed switch when it is created.  (As in:
the reflexive transitive closure of the dependencies listed in
`dune-project`.)

Say you want to add `cryptokit` as a dependency.  (We aren't using
`cryptokit`, so this example is fictional.)  First add it to the
`package` s-expression in `dune-project` under `depends`:

```dune
(package
 (name attachment-converter)
 (synopsis "does something")
 (description "cf. synopsis")
 (depends
  (ocaml (>= 4.12.0))
  dune
  prelude
  ocamlbuild
  ocamlfind
  mrmime
  ocamlnet
  crypotkit
  (qtest :dev)
  (utop :dev)
  (ocamlformat :dev)
  (ocp-index :dev)
  (merlin :dev)))
```

Next, install it to whatever switch you're currently in (this is
necessary for running `opam-lock`):

```
$ opam install cryptokit
```

Next, compile the project to regenerate the `.opam` file:

```
$ make
```

Finally, run `opam-lock` to update the `.locked` file:

```
$ opam lock ./attachment-converter.opam
```

Commit all of these config files:

```
dune-project
attachment-converter.opam
attachment-converter.opam.locked
```

And now anyone who pulls your branch down will be able to create a
sandboxed `opam` switch just for Attachment Converter on their
machine.
