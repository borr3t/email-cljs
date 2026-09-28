#!/usr/bin/env bash
# Compiles the cljs.test suite (shadow-cljs :node-test build) and runs it.
# Exit status: 0 when every test passes, non-zero otherwise.
#
# The build has :autorun true (tests execute on every compile/watch), but
# shadow-cljs does not propagate the autorun exit status, so we disable autorun
# for this one compile and run the generated script ourselves.
#
# CAVEAT: the last line is `exec` — this script REPLACES its shell with node, so
# never chain it mid-command (`./run-tests.sh && something` is fine from a parent
# shell, but nothing placed after the exec line inside this file would run).
#
# The tests never touch the network or the secrets file: `npm install` (first
# run only) fetches the toolchain plus nodemailer/imapflow/mailparser; SMTP and
# IMAP transports are stubbed (nodemailer's jsonTransport / streamTransport and
# fake imapflow objects), message parsing runs in-process.
#
# The publicity gate runs FIRST: src/ and README.md are public documentation.
set -euo pipefail
cd "$(dirname "$0")"

if [ ! -d node_modules ]; then
  npm install
fi

bash test/publicity_gate.sh

npx shadow-cljs compile node-test --config-merge '{:autorun false}'
exec node target/node-test.js
