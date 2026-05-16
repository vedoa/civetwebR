#!/usr/bin/env bash
set -e

# Base directory: go two levels up from inst/civetweb
BASE_DIR="$(cd "$(dirname "$0")/../.." && pwd)"

# Target directories relative to package root
TARGET_DIR="$BASE_DIR/src"
LICENSE_DIR="$BASE_DIR/inst/civetweb"

mkdir -p "$TARGET_DIR"
mkdir -p "$LICENSE_DIR"

echo "Downloading CivetWeb files into $TARGET_DIR ..."

# Helper function
download() {
  local url=$1
  local out=$2
  echo " $out"
  curl -L "$url" -o "$out"
}

# ---- Core files ----

download \
"https://raw.githubusercontent.com/civetweb/civetweb/d110e8731c6aa0fcd04504deb449bdb773cfb9da/src/civetweb.c" \
"$TARGET_DIR/civetweb.c"

download \
"https://raw.githubusercontent.com/civetweb/civetweb/9bbef31027c1c7bccddae122abcd92a5ec8e0375/include/civetweb.h" \
"$TARGET_DIR/civetweb.h"

# ---- INL files ----

download \
"https://raw.githubusercontent.com/civetweb/civetweb/63ef070ca5a5e21c7a4a56233790596b2d56891c/src/md5.inl" \
"$TARGET_DIR/md5.inl"

download \
"https://raw.githubusercontent.com/civetweb/civetweb/6ca12def93eb1b5f61642d969495a9a8ab37067a/src/sort.inl" \
"$TARGET_DIR/sort.inl"

download \
"https://raw.githubusercontent.com/civetweb/civetweb/8db42679a4b52b2ebcc10cf24da3777b369d84e5/src/match.inl" \
"$TARGET_DIR/match.inl"

download \
"https://raw.githubusercontent.com/civetweb/civetweb/c6a212dcdf8fe56f36297ab5ad7e2f8cb613b522/src/response.inl" \
"$TARGET_DIR/response.inl"

download \
"https://raw.githubusercontent.com/civetweb/civetweb/cafd5f8fae3b859b7f8c29feb03ea075c7221497/src/handle_form.inl" \
"$TARGET_DIR/handle_form.inl"

# ---- License ----

download \
"https://raw.githubusercontent.com/civetweb/civetweb/3e7eceddeb626f5195f7c813b788a118c20eda1e/LICENSE.md" \
"$LICENSE_DIR/LICENSE.md"

echo "All CivetWeb files downloaded successfully into package root."