Test for attc --help command
  $ attc --help > /dev/null
  $ echo "Exit Code:" $(echo $?)
  Exit Code: 0
