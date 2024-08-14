The process for updating the PPA will be as follows:
1. git pull updates to the source code
1.5. Add/update the opampack.tar.gz if necessary into the source directory
     Run opampack.sh from the root of the project to generate the opampack tarball (which includes the switch and source for opam dependencies)
2. cd out of the project, retar the project, rename it attachment-converter_ver_num.orig.tar.gz (and leave the new tarball one directory up from the root of the project)
3. cd back into the project and run `debuild -S` (to prepare for the PPA) or pdebuild to test building binaries
4. cd back out of the project and run `dput ppa:uchicago-dldc/attc <source.changes>` 

To contribute to the DLDC Launchpad Team, have an admin add you with some sort of level of permission, perform Launchpads biggest wishes (sign their user agreement with a GPG key, upload your GPG key) and then maybe perhaps you can contribute to the team
s PPA

Eventually this directory will contain the debian directory which contains the following files:
1. attc.dirs (which specifies necessary installation directories like /usr/bin and /usr/lib/attachment-converter/scripts)
2. attc.manpages (which specifies which directory the man page is in the project dir)
3. changelog (which contains IMPORTANTLY the version number and distribution type)
4. control (which contains build time and run time dependencies)
5. copyright (which contains copyright)
6. files (TODO: Add description)
7. rules (debian's makefile which contains build, install, and clean targets
8. source (some directory that debian really wants for some reason)

TODO:
Fix the opampack.sh script
Move files into this branch
Copy the debian directory to root of project
Expanation for opampack (ubuntu wants networkless environment, blah blah blah)
Think harder about step 1
