#!/usr/bin/env bash

function msg () {
  LEN=$(echo -n $1 | wc -c)
  echo 'Content-Length: '
}

{ sleep 1s;
  echo IHIHIHIHIH >> asdf.log;
    echo -n 'Content-Length: 196'$'\r
\r
''{"jsonrpc":"2.0","id":1,"result":{"capabilities":{"textDocumentSync":2,"definitionProvider":true,"documentSymbolProvider":true,"completionProvider":{"resolveProvider":true},"hoverProvider":true}}}'; } &


{ sleep 2s;
  echo IHIHIHIHIH >> asdf.log;
    echo -n 'Content-Length: 172'$'\r
\r
''{"jsonrpc":"2.0","id":0,"method":"window/showMessageRequest","params":{"type":1,"message":"Choose an option, 1 or 2","actions":[{"label":"Option 1"},{"label":"Option 2"}]}}'; } &

while read msg; do
  echo "$msg" >>asdf.log
done
