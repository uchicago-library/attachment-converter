## Updating the PPA
1. As always, create a tag for the new version by running the commands `git tag VER_NUM` and `git push origin VER_NUM`.
Ensure that the `debian/changelog` file is properly updated with the **correct ubuntu distribution** and the new **version number**.  Ensure that `lib/version.ml` is similarly updated
2. On a machine capable of running `debuild` and `dput`, retrieve the project by running `wget -c https://github.com/uchicago-library/attachment-converter/archive/VER_NUM/attachment-converter-VER_NUM.tar.gz`
3. Untar the retrieved tar ball and `cd` into the project
4. For the `opampack` process:
    1. `cd` into `ubuntu_wsl`
    2. Run `./Opampack.sh`
    3. `cd` into the directory above the root of the project (`cd ../..`)
    4. Run `tar czf attachment-converter_VER_NUM.orig.tar.gz` (The underscore must precede the `VER_NUM` and the `.orig.tar.gz` is necessary)
    5. `cd` back into the project directory
5. Run `debuild -S` or `debuild -S -k"GPG_KEY"` (There is no space between `-k` and the specified key) to generate necessary files for uploading to the PPA as well as signing the files with the correct GPG Key.
6. `cd` back out of the project to locate the `source.changes` file and run `dput ppa:uchicago-dldc/attc <source.changes>`
7. Ensure the package has been uploaded to Launchpad. Allow approximately one hour for Laucnhpad to build the package and upload the package for download.
    You can monitor the build progress by selecting `View Package Details`, selecting the newly uploaded package and pressing on the current Build. 
    After building, Launchpad will also have a pop-up informing you that the binary has been built but not yet uploaded. Once this notice dissapears, the package is uploaded to the PPA
8. After this process, users using the correct ubuntu distribution can download the package by running:
    `sudo add-apt-repository ppa:uchicago-dldc/attc`
    `sudo apt update`
    `sudo apt install attc`
    It may then become necessary to upload the package multiple times for different distributions

To contribute to the DLDC Launchpad Team, have an admin add you with permission to upload packages, perform Launchpads biggest wishes (upload your GPG key, sign their user agreement with the same GPG key) and then it possible to contribute

## Debian Files
The `debian` directory is necessary for debian packages. It contains the following files:
- `attc.dirs` specifies directories necessary for installation like `/usr/bin` and `/usr/lib/attachment-converter/scripts`
- `attc.manpages` specifies which directory the man page is located in the project
- `changelog` contains the **version number** and **distribution type**. This file *must* be updated for each new release.
- `control` contains build time and run time dependencies. Should conversion utilities change, this file must be changed
- `copyright` contains copyright information
- `rules` is debian's makefile which contains build, install, and clean targets. Rules has been configured to override some debian specific targets that fail when running `debuild`. It will likely be unecessary to modify this file often.
- `source` contains the `format` file which specifies which patch utility to use. This can largely be ignored
More information about all of these files can be found [here](https://www.debian.org/doc/manuals/maint-guide/dreq.en.html)

## FAQ:
#### Why does the opampack process exist?
Launchpad desires the source of the project (hence the `-S` in the `debuild` command). Launchpad chooses to build binaries on their build farm and then upload it those built binaries to PPA's entirely on their end. However, they choose to build in a network-less environment. This means that downloading necessary `opam` dependencies during the build is impossible. `OpamPack.sh` creates a simulated opam environment with downloaded source for each necessary dependency.

#### What about Keith's Prelude?
The `opam` file for Prelude is included in this directory and is moved into the correct position when `OpamPack.sh` is run

#### `dput` says my file uploaded properly but it won't appear on launchpad
Launchpad will often send an email explaining why a package was accepted or rejected with an error. It may be that the version number has already been used or the distribution type may be invalid. If no email was received, it was silently rejected likely due to GPG reasons. Confirm that the correct GPG key is being used to upload, that same GPG key was uploaded to Launchpad, and that the user agreement was signed.
If fixing a release for the same version, change the package version by incrementing the package version in the `changelog` file i.e. if the version number was `1.0.0-1` change it to `1.0.0-2` where `1.0.0` is the source package version and `-1` and `-2` indicate the version for the packaging process

#### Why does the version number look like that?
The version number will contain the source version number, the package release number, and then the ubuntu distribution. For example, `0.1.1-2~jammy` indicates project version `0.1.1`, the second package release for that project version, and the ubuntu distribution jammy jellyfish.

#### Why not package for debian?
Submitting a package to the official `apt` repositories requires a lengthy vetting process and subsequent version releases also need to undergo such vetting. The alternative, a third-party `apt` repository, requires finicky and needlessly complicated GPG key configuration for the user. PPA's present a smoother end user experience by removing the need to deal with tricky GPG configuration and add an `apt` repository through a simple command. 

#### Why is this directory specificed as a `data_only_dirs`?
The `opam` switch being hosted within this directory means that during the build process, binaries and executables are built and placed within subdirectories of this directory to be executed by `opam`. This includes a `dune` executable that is mistaken for a `dune` file. Specifying this directory as a `data_only_dirs` avoids that error.