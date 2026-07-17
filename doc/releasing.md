# Release checklist

This is a guide for DLDC developers who would like to push out a fresh
release of Attachment Converter.  See [background](#background)
section for more background.

## Updating the `attachment-converter` repository

Prep the repository before creating a fresh release tag:

- [ ] create sandboxed switch and switch into it
- [ ] run `dune build` to update the `attachment-converter.opam` file
- [ ] (if it's an upstream version bump) update `VER_NUM` in `GNUmakefile` to a new version number of your choice
- [ ] (if it's a revision bump) update `REVISION` in `GNUmakefile` to a new revision number of your choice
- [ ] commit `GNUmakefile` and `attachment-converter.opam`
- [ ] run `make prep-for-release`
- [ ] review/commit the changes
- [ ] push up to `main`

`make prep-for-release` will update the upstream version in
`PKGBUILD`, update the revision in `PKGBUILD`, and
`Lib.Version.ver_num`, then pull down the latest Prelude `opam` file,
then generate fresh `opampack-packs` and `opampack-upacks` files.
This should take about 3 minutes, since generating fresh opampack
files requires building a fresh switch.

## Arch release

From a machine that is capable of publishing to the DLDC Arch Linux
software repository on `staff.lib`, run `make arch-release`.  This
rule will build the project in a `mktemp`-ed directory and copy all
relevant files up to our DLDC Arch Linux repository.

Note: `make arch-release` creates a general-purpose release tarball
and `rsync`-s it over to `dldc.lib.uchicago.edu/open/dist/attc`, which
will end up being the same tarball URL our `homebrew-attc` repository
points to.  This means that we recommend updating the Homebrew formula
*after* running `make arch-release`, whenever we are in control of the
order in which those two things happen.

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

## Ubuntu release

Unfortunately, Debian forces us to follow two different fragile,
byzantine workflows, depending on whether or not we are bumping the
upstream version number or the revision number.

### If you are bumping the upstream version number

From a machine with an emulated Ubuntu environment, with `gpg` set up
to use the UChicago DLDC private key:

- [ ] enter the Ubuntu environment, running under `qemu`
- [ ] cd `/path/to/cloned/repo`
- [ ] pull down changes from `main`
- [ ] run `make launchpad`
- [ ] run `make debian-dist-publish`

In the course of running `make launchpad`, you will first be prompted
to enter the `sudo` password for the emulated ubuntu account.  Then,
after a few minutes, when the rule runs the `debuild` utility, you
will be prompted to enter the UChicago DLDC's `gpg` key password.
After that, it is safe to get up and make a coffee etc. while `make
launchpad` runs in the background.  The whole process takes 10-15
minutes, because this rule concludes by trying to build the entire
project in a chroot using Debian's `sbuild` utility.  `make launchpad`
concludes by uploading the project to Debian launchpad, using Debian's
`dput` utility.

The `make debian-dist-publish` rule copies these files to our public
webspace at `dldc.lib.uchicago.edu/open/dist/attc`:

- `attc_0.2.10-4~resolute_amd64.deb`
- `attachment-converter_0.2.10.orig.tar.gz`

(With whatever the correct version and revision numbers are, of
course.)

### If you are bumping the revision number

From a machine with an emulated Ubuntu environment, with `gpg` set up
to use the UChicago DLDC private key:

- [ ] enter the Ubuntu environment, running under `qemu`
- [ ] cd `/path/to/cloned/repo`
- [ ] pull down changes from `main`
- [ ] run `make launchpad-revision`
- [ ] run `make debian-dist-publish`

As in the previous section, you will first be prompted to enter the
`sudo` password for the emulated ubuntu account, then, 2-3 minutes
later, for the UChicago DLDC's `gpg` key password.  After that, the
`make launchpad-revision` can be left to run in the background, and
should take about 10-15 minutes to finish.

The `make debian-dist-publish` rule behaves the same as in the
previous section, copying these files to our public webspace at
`dldc.lib.uchicago.edu/open/dist/attc`:

- `attc_0.2.10-4~resolute_amd64.deb`
- `attachment-converter_0.2.10.orig.tar.gz`

(With whatever the correct version and revision numbers are.)

### The aftermath

The UChicago DLDC is set up with a PPA on Debian launchpad, with the
UChicago DLDC `gpg` key as its public key and Matt Teichman as an
admin.  It takes Launchpad 1-2 hours to build the project.  It
notifies you by email when you first upload it with `dput`.  If the
job fails, it will send you an email notification.  If the job
succeeds, you won't get a second email notification.

Once Launchpad has built and deployed the package, the user should be
able to install `attc` by running the following commands:

```console
$ sudo add-apt repository ppa:uchicago-dldc/attc
$ sudo apt update
$ sudo apt install attc
```

Ubuntu will prompt the user to confirm that they accept our PPA's
`gpg` public key the first time they add the PPA.
