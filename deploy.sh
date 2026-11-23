#!/usr/bin/env bash
set -euo pipefail
repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
target_home=$HOME
platform=auto
action=${1:-help}
[[ $# -eq 0 ]] || shift
packages=()
while [[ $# -gt 0 ]]; do
    case $1 in
        --home|--platform)
            [[ $# -ge 2 ]] || { echo "Missing value: $1" >&2; exit 1; }
            if [[ $1 == --home ]]; then target_home=$2; else platform=$2; fi
            shift 2 ;;
        --packages) shift; packages=("$@"); break ;;
        *) echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done
case $action in
    help|-h|--help)
        echo 'deploy.sh check|install|status|uninstall [--home PATH] [--platform auto|darwin|linux] [--packages NAME ...]'
        exit 0 ;;
    deploy) action=install ;;
    doctor) action=status ;;
    check|install|status|uninstall) ;;
    *) echo "Unknown action: $action" >&2; exit 1 ;;
esac
[[ -d $target_home ]] || { echo "Target must be an existing directory: $target_home" >&2; exit 1; }
target_home=$(cd -- "$target_home" && pwd -P)
if [[ $platform == auto ]]; then
    case $(uname -s) in Darwin) platform=darwin ;; Linux) platform=linux ;; esac
fi
[[ $platform == darwin || $platform == linux ]] || { echo "Unsupported platform: $platform" >&2; exit 1; }

# Explicit packages: tooling, caches and system-level hibernation are excluded.
common=(ag emacs git kitty mise nvim ranger ripgrep scripts snippets tmux uctags vim yazi zsh)
mac=(hammerspoon herdr skhd slate)
linux=(i3 mate rofi sway swaync systemd waybar wofi)
allowed=("${common[@]}" "${mac[@]}" "${linux[@]}")
if [[ ${#packages[@]} -eq 0 ]]; then
    packages=("${common[@]}")
    if [[ $platform == darwin ]]; then packages+=("${mac[@]}"); else packages+=("${linux[@]}"); fi
fi
for package in "${packages[@]}"; do
    valid=false
    for name in "${allowed[@]}"; do [[ $package != "$name" ]] || valid=true; done
    [[ $valid == true && -d $repo_dir/$package ]] || { echo "Unknown or missing package: $package" >&2; exit 1; }
done
stow_args=(--no-folding --verbose=1 --dir "$repo_dir" --target "$target_home" '--ignore=(^|/)(node_modules|__pycache__|\.DS_Store)$')

if [[ $action == uninstall ]]; then
    stow "${stow_args[@]}" --simulate --delete "${packages[@]}"
    stow "${stow_args[@]}" --delete "${packages[@]}"
elif [[ $action == status ]]; then
    output=$(stow "${stow_args[@]}" --simulate --stow "${packages[@]}" 2>&1) || { echo "$output" >&2; exit 1; }
    if [[ $output == *'LINK:'* || $output == *'MKDIR:'* ]]; then echo "$output"; exit 1; fi
    echo 'Managed links are installed.'
else
    stow "${stow_args[@]}" --simulate --restow "${packages[@]}"
    if [[ $action == check ]]; then echo 'Check passed; no changes made.'; exit 0; fi
    stow "${stow_args[@]}" --restow "${packages[@]}"
fi
