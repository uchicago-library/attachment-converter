# Release checklist

This is a guide for DLDC developers who would like to push out a fresh
release of Attachment Converter.  See [Background](#background)
section for more background.

## Creating the new version of `attc` in the `attachment-converter` repository

Prep the repository before creating a fresh release tag:

- [ ] create sandboxed switch and switch into it
- [ ] run a build to update the `attachment-converter.opam` file
- [ ] bump `VER_NUM` Make variable in `GNUmakefile`
- [ ] bump `pkgver` shell variable in `arch/PKGBUILD`
- [ ] bump version number in first line of `debian/changelog`
- [ ] make sure the Debian changelog mentions the correct version of
      Debian (e.g. `resolute` vs. `jammy`)
- [ ] `date -R` to update the Debian changelog timestamp
- [ ] check that `PRELUDE_VER_NUM` is up to date in `GNUmakefile`
- [ ] run `make prelude` to pull the latest Prelude `opam` file down
- [ ] run `make opampack` to update package list files,
      `ubuntu_wsl/opampack-packs` etc. (warning: this is slow)
- [ ] update `Formula/attc.rb` in `homebrew-attc` repo, version number
      of tarball in `url` field

Create the new release tag:

- [ ] create a new commit in this project with those changes
- [ ] give that commit a release tag with the latest version
- [ ] push the new commit up to `origin/main`
- [ ] push the new tag up to `origin/main`

After creating the new release tag:

- [ ] get the new checksum by running `make arch-checksum`
- [ ] insert the new checksum into the `sh256sums` field of
      `arch/PKGBUILD`
- [ ] in the `homebrew-attc` repository, insert the new checksum into
      the `sha256` field of `Formula/attc.rb`

## Homebrew Release

Nothing further is required for releasing to Homebrew.  The user
should be able to do the following on their machine to install `attc`:

```console
$ brew tap uchicago-library/attc
$ brew install attc
```
## Arch Release

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

## Ubuntu Release

- [ ] enter the Ubuntu environment, running under `qemu`
- [ ] cd `~/tmp`
- [ ] `wget -c https://github.com/uchicago-library/attachment-converter/archive/v0.1.44/attachment-converter-v0.1.44.tar.gz`
      (replace `0.1.44` with actual version number)
- [ ] `tar xzvf attachment-converter-v0.1.44.tar.gz` (replace `0.1.44` with actual version number)
- [ ] cd `attachment-converter-v0.1.44/ubuntu_wsl`
- [ ] `./OpamPack.sh`
- [ ] `cd ../..`
- [ ] `tar czf attachment-converter_0.1.44.orig.tar.gz attachment-converter-v0.1.44`
- [ ] it must be `attachment-converter_VER_NUM.orig.tar.gz` with an
      underscore (not a hyphen) and then the number with no preceding
      `v`
- [ ] `cd ..`
- [ ] `gpg --list-keys` to display DLDC repo's public `gpg` key
- [ ] `debuild -S -k"3EF45886DF1EF82B4782F5FBD331DB7453444E0E"`
- [ ] you will be prompted to enter the DLDC repo's private `gpg` key
- [ ] `debuild` must be run from the root of the project
- [ ] `cd ..`
- [ ] `dput ppa:uchicago-dldc/attc attachment-converter_0.1.44-1~resolute_source.changes`
- [ ] `dput` command must be run from one directory up from the project root

Our account on Debian Launchpad is set up, and the UChicago DLDC `gpg`
key is associated with that account. It takes Launchpad 1-2 hours to
build the project.  It notifies you by email when you first upload it
with `dput`.  If the job fails, it will send you an email
notification.  If the job succeeds, you won't get a second email
notification.

The user should then be able to install `attc` by running the following commands:

```console
$ sudo add-apt repository ppa:uchicago-dldc/attc
$ sudo apt update
$ sudo apt install attc
```

Ubuntu will prompt the user to confirm that they accept our PPA's
`gpg` public key the first time they add the PPA.

# Background

text

