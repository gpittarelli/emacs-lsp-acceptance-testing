#!/usr/bin/env bash

sleep 0.5s

echo -n 'Content-Length: 196'$'\r
\r
''{"jsonrpc":"2.0","id":1,"result":{"capabilities":{"textDocumentSync":2,"definitionProvider":true,"documentSymbolProvider":true,"completionProvider":{"resolveProvider":true},"hoverProvider":true}}}'

sleep 1s;
echo -n 'Content-Length: 172'$'\r
\r
''{"jsonrpc":"2.0","id":0,"method":"window/showMessageRequest","params":{"type":1,"message":"Choose an option, 1 or 2","actions":[{"title":"Option 1"},{"title":"Option 2"}]}}'

# stay alive so owning process doesn't whine about us dieing
while read msg; do
  sleep 0.1s;
done
