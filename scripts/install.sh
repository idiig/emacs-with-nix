#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat >&2 <<'USAGE'
Usage: install.sh <aarch64-darwin|x86_64-linux> [repo]

Downloads the latest portable Emacs release, imports it into the local
Nix store, and launches it. Requires: gh, nix (with a running
nix-daemon), zstd, tar.

  repo    defaults to idiig/emacs-with-nix
USAGE
  exit 1
}

[ $# -ge 1 ] || usage
system="$1"
repo="${2:-idiig/emacs-with-nix}"

case "$system" in
  aarch64-darwin) prefix="emacs-arm64" ;;
  x86_64-linux)   prefix="emacs-x86_64" ;;
  *)
    echo "Unsupported system: $system (expected aarch64-darwin or x86_64-linux)" >&2
    exit 1
    ;;
esac

for cmd in gh zstd tar nix-store; do
  command -v "$cmd" >/dev/null 2>&1 || {
    echo "Missing required command: $cmd" >&2
    exit 1
  }
done

tag="$(gh release list -R "$repo" --limit 1 --json tagName -q '.[0].tagName')"
[ -n "$tag" ] || { echo "No release found for $repo" >&2; exit 1; }
echo "Latest release: $tag"

work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT

# gh's HTTP/2 connection sometimes idles out mid-batch on a run of
# large sequential downloads, leaving a truncated part on disk -- so
# each retry uses --clobber and re-downloads cleanly rather than
# trusting whatever is already there, and success is judged by the
# archive actually decompressing, not just gh's exit code.
shopt -s nullglob
max_attempts=5
attempt=1
while :; do
  echo "Downloading $prefix parts (attempt $attempt/$max_attempts) ..."
  gh release download "$tag" -R "$repo" -D "$work_dir" \
      -p "${prefix}-*.tar.zst.part-*" --clobber || true

  parts=("$work_dir/${prefix}"-*.tar.zst.part-*)
  [ ${#parts[@]} -gt 0 ] || {
    echo "No matching parts downloaded for $system -- wrong architecture or release naming changed?" >&2
    exit 1
  }

  archive="$work_dir/${prefix}.tar.zst"
  cat "${parts[@]}" > "$archive"

  echo "Extracting ..."
  if (cd "$work_dir" && zstdcat "$archive" | tar xf -); then
    break
  fi

  echo "Extraction failed on attempt $attempt (likely a truncated download) ..." >&2
  rm -f "${parts[@]}" "$archive"
  attempt=$((attempt + 1))
  [ "$attempt" -le "$max_attempts" ] || {
    echo "Giving up after $max_attempts attempts" >&2
    exit 1
  }
  sleep 3
done

echo "Importing into the Nix store ..."
# Some paths (e.g. locally-built texlive derivations) were never
# fetched from a signed binary cache, so they carry no trusted
# signature; require-sigs=false is scoped to this one import.
nix-store --import --option require-sigs false < "$work_dir/emacs-closure.nar"

install_dir="$HOME/.local/share/emacs-with-nix"
mkdir -p "$install_dir"
result_path="$(readlink "$work_dir/result")"
nix-store --realise "$result_path" --add-root "$install_dir/$tag" --indirect >/dev/null
ln -sfn "$install_dir/$tag" "$install_dir/current"

echo "Installed: $install_dir/current/bin/emacs"
exec "$install_dir/current/bin/emacs"
