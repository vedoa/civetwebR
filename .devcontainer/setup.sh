#!/usr/bin/env bash
set -e 

apt-get update && rm -rf /var/lib/apt/lists/*
R --no-init-file -e 'install.packages("languageserver")'
curl -LsSf https://github.com/posit-dev/air/releases/download/0.9.0/air-installer.sh | sh
