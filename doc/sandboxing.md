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
on my machine' situation.  If we're all developing in the same
environment, and a user can recreate our exact dev environment on
their machine in one command, then they will be able to work on our
project (if they want to) without getting stuck in config hell.

## The DLDC Opam Repo

We have now created an `opam` repository that is world-readable at:

https://dldc.lib.uchicago.edu/opam

This repository contains our in-house libraries, which at this point
mainly means `prelude`, our standard library for OCaml.

## Workflow

Our workflow for contributions at the level of the sandboxed `opam`
switch breaks up into two parts: creating a new switch, and updating
any changes you've made.

### Creating a New Switch

To create a new switch
