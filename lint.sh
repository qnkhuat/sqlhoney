#! /usr/bin/env bash
set -euo pipefail
clj-kondo --copy-configs --dependencies --lint "$(clojure -A:dev -Spath)" --skip-lint --parallel

clj-kondo --parallel --lint src test
