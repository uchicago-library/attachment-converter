# Release checklist

This is a guide for DLDC developers who would like to push out a fresh
release of Attachment Converter.  See [background](#background)
section for more background.

## Updating the `attachment-converter` repository

Prep the repository before creating a fresh release tag:

- [ ] create sandboxed switch and switch into it
- [ ] run `dune build` to update the `attachment-converter.opam` file
- [ ] update `VER_NUM` in `GNUmakefile` to a new version number of your choice
- [ ] update `REVISION` in `GNUmakefile` to a new revision number of your choice
- [ ] commit `GNUmakefile` and `attachment-converter.opam`
- [ ] run `make prep-for-release`

`make prep-for-release` will update the version in `PKGBUILD`,
`Lib.Version.ver_num`, and the version number in `debian/changelog`,
then pull down the latest Prelude `opam` file, then generate fresh
`opampack-packs` and `opampack-upacks` files.  *Warning: it will run
`git` commands.*  It will commit all these changes, tag the commit
with a release tag corresponding to `VER_NUM`, and push both the
commit and the tags up to `origin/master`.  This should take about 3
minutes, since generating fresh opampack files requires building a
fresh switch.

## Homebrew release

First, update what needs to be updated in the `homebrew-attc` repository:

- [ ] get the latest checksum by running `make arch-checksum`
- [ ] update `Formula/attc.rb` in `homebrew-attc` repo, version number
      of tarball in `url` field
- [ ] in the `homebrew-attc` repository, insert the new checksum into
      the `sha256` field of `Formula/attc.rb`

With these changes, the user should be able to do the following on
their machine to install `attc`:

```console
$ brew tap uchicago-library/attc
$ brew install attc
```
## Arch release

From a machine that is capable of publishing to the DLDC Arch Linux
software repository on `staff.lib`, run `make arch-release`.

The user should be able to then install `attc` by first adding this to
their `/etc/pacman.conf`:

```ini
[dldc]
SigLevel = Optional TrustAll
Server = http://dldc.lib.uchicago.edu/open/repos/arch
```

Then to install:

```console
$ sudo pacman -Syy
$ sudo pacman -S attc
```

The user will have to confirm that they accept the UChicago DLDC Arch
Linux repository's `gpg` public key after they run `pacman -Syy`.

## Ubuntu release

From a machine with an emulated Ubuntu environment, with `gpg` set up
to use the UChicago DLDC private key:

- [ ] enter the Ubuntu environment, running under `qemu`
- [ ] cd `/path/to/cloned/repo`
- [ ] run `make launchpad`

In the course of running `make launchpad`, you will be prompted to
enter the UChicago DLDC's `gpg` key password.  Our account on Debian
Launchpad is set up, and the UChicago DLDC `gpg` key is associated
with that account. It takes Launchpad 1-2 hours to build the project.
It notifies you by email when you first upload it with `dput`.  If the
job fails, it will send you an email notification.  If the job
succeeds, you won't get a second email notification.

The user should then be able to install `attc` by running the
following commands:

```console
$ sudo add-apt repository ppa:uchicago-dldc/attc
$ sudo apt update
$ sudo apt install attc
```

Ubuntu will prompt the user to confirm that they accept our PPA's
`gpg` public key the first time they add the PPA.
