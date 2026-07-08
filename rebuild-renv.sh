#!/bin/bash
# Run this from the project root to rebuild renv.lock inside a matching Docker container.
# Use this whenever you need to add/update packages without having R installed locally.

docker run --rm \
  -v "$(pwd):/project" \
  -w /project \
  rocker/r-ver:4.5.2 \
  bash -c "
    apt update -q && \
    apt install -y -q \
      libcairo2-dev \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libfribidi-dev \
      libharfbuzz-dev \
      libjpeg-dev \
      libpng-dev \
      libssl-dev \
      libtiff5-dev \
      libxml2-dev && \
    Rscript -e '
      renv::restore()
      # renv::install(\"some_package\")  # uncomment and edit to add new packages
      # renv::record(\"some_package\")   # records packages not referenced in .R files (e.g. rsconnect)
      renv::snapshot(prompt = FALSE)    # records packages that are used in .R files
    '
  "
