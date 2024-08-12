After creating the release tag, make sure to update the ver_num on the URL to the tar ball as well as update the SHA sum

Command for fetching the SHA sum:
curl -sL "https://github.com/benkim04/attachment-converter/archive/refs/tags/ver_num.tar.gz" | shasum -a 256 | cut -d " " -f 1

