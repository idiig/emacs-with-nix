#!/usr/bin/env bash
set -euo pipefail

# Build the wrappedEmacs closure and export it as a portable tar.zst
# archive (Nix store export/import, see packages.emacs in flake.nix).
# Target machine must already have Nix installed and match this
# machine's OS/architecture.

# claude-agent-acp/codex-acp are unfree packages; nixpkgs only reads
# NIXPKGS_ALLOW_UNFREE via impure evaluation, so --impure is required.
export NIXPKGS_ALLOW_UNFREE="${NIXPKGS_ALLOW_UNFREE:-1}"

cd "$(dirname "$0")/.."

out_dir="dist"
mkdir -p "$out_dir"

work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT

# Build result inside work_dir so the nar and the result symlink share
# one directory and can be tar'd together with a single -C.
nix build .#emacs -o "$work_dir/result" \
  --impure \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes

nix-store -qR "$work_dir/result" > "$work_dir/paths.txt"
nix-store --export $(cat "$work_dir/paths.txt") > "$work_dir/emacs-closure.nar"

archive="$out_dir/emacs-$(uname -m)-$(date +%Y%m%d).tar.zst"
tar cf - -C "$work_dir" emacs-closure.nar result | zstd -19 -T0 > "$archive"

# GitHub Release rejects assets >= 2GiB, but in practice uploads of
# ~1.9GB parts also got hard-rejected ("Whoa there") well below that
# documented limit -- looks like an edge/WAF body-size cutoff, not the
# application-level check. 500M parts upload reliably. Reassemble with:
# cat "$archive".part-* > "$archive"
split -b 500M -d -a 2 "$archive" "$archive.part-"
rm "$archive"

echo "Bundled parts:"
ls -la "$out_dir"
