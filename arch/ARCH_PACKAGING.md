## Updating the PKGBUILD
1. As always, create a tag for the new version by running the commands `git tag VER_NUM` and `git push origin VER_NUM`. Once the tag is pushed to github, github will automatically create a tarball of the whole project that can be obtained from a link.
2. Fetch the SHA sum for the new tag by performing the following command:
`curl -sL "https://github.com/uchicago-library/attachment-converter/archive/refs/tags/VER_NUM.tar.gz" | shasum -a 256 | cut -d " " -f 1`
3. Within this directory lies the PKGBUILD file. Modify line 4 to the correct version number and line 13 with the new sha sum.
4. Users can then use hits PKGBUILD file like how other ARCH packages are build and installed.

TODO:
- figure out all the ins and outs of hosting this in our DLDC arch linux repo