#!/usr/bin/env bash
# Builds the docs site. Two tools: quarto does the home + guides, pkgdown does
# the function reference off the roxygen docs. Quarto wipes _site/ on every
# render, so run it first, then drop pkgdown's reference into _site/reference.
# Safe to copy -- pkgdown keeps its assets under deps/ and writes no search.json
# at the root, so it doesn't clobber quarto's files.
#
# No quarto on PATH on the SCC, so point pkgdown at the module's quarto and the
# pandoc it ships with.
#
##TODO move to whole-site pkgdown once the callouts are gone (see TODO.md)

set -euo pipefail
cd "$(dirname "$0")"

module load quarto 2>/dev/null || true
quarto_bin="$(command -v quarto || echo /share/pkg.7/quarto/1.2.313/install/bin/quarto)"
export QUARTO_PATH="$quarto_bin"
export RSTUDIO_PANDOC="$(dirname "$quarto_bin")/tools"

rscript="${RSCRIPT:-/share/pkg.8/r/4.5.2/install/bin/Rscript}"

# home + guides
"$quarto_bin" render

# function reference
"$rscript" -e 'pkgdown::init_site(); pkgdown::build_reference()'

# stitch the reference into the quarto site
cp -r docs/. _site/

echo "done, open _site/index.html"
