#!/usr/bin/env bash

set -eu

dir="${OUT:-$(mktemp -d)}"
trap 'rm -rf ${dir}' EXIT ERR
cp "misc/style.css" "${dir}/" || true
${HERMS:-herms} export --format html >> "${dir}/index.html"

printf "Serving from %s...\n" "${dir}"
cd "${dir}"
ls
python -m http.server 9000
printf "Removing %s...\n" "${dir}"
