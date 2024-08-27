## Updating the Homebrew Formula
1. As always, create a tag for the new version by running the commands `git tag VER_NUM` and `git push origin VER_NUM`. Once the tag is pushed to github, github will automatically create a tarball of the whole project that can be obtained from a link. Ensure that `lib/version.ml` is updated with the correct **version number**.
2. Fetch the SHA sum for the new tag by performing the following command:
`curl -sL "https://github.com/uchicago-library/attachment-converter/archive/refs/tags/VER_NUM.tar.gz" | shasum -a 256 | cut -d " " -f 1`
3. Within this directory lies the example Formula. Fill in the field for `sha256` on line 5 and modify the `VER_NUM` in the `url` field on line 4 to fetch the correct tag.
4. Update `Formula/attc.rb` in the uchicago-library repository named `homebrew-attc`.
5. Users can then download `attc` by performing following:
`brew install benkim04/attc/attc`
OR
`brew tap benkim04/attc` and then `brew install attc`

## FAQ:
#### Why does `configuration.ml` have three different possible paths for the scripts?
Homebrew is available for both ARM and x86. Homebrew also uses different paths for both architetures. These paths differ from the directories used by both debian and ARCH linux. As such, `attc` will search all three possible desetinations for the scripts necessary to convert attachments.
ARM based machines use the `/opt/homebrew` path while x86 machines use `usr/local`

#### Why does `lib/version.ml` exist?
This file provides the **version number** for the release of attachment converter. This is necessary specifically for homebrew installations as the path towards the conversion scripts is contained within the version specific directory for `attc` (`Cellar/attc/VER_NUM/lib/`). In order for `attc` to find the scripts on machines with homebrew, the version number in `lib/version.ml` **must match** the version number in the `url` field and thus the tag name of that release.

TODO:
- make the path to the conversion scripts in attc configurable
- somehow or other, I know not how, make the Mac version use the homebrew path
- have attc read the path to the conversion scripts outta the config