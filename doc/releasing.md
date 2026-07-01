# Release checklist

## Creating the new version of `attc` in the `attachment-converter` repository

Prep the repository for creating a fresh release tag:

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
- [ ] push the new commit up
- [ ] push the new tag up

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


## Ubuntu Release

- [ ] enter the Ubuntu environment, running under `qemu`
- wget command
- 

