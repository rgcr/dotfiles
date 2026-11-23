#!/usr/bin/env bash
set -euo pipefail

repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
die() {
    printf '%s\n' "$*" >&2
    exit 1
}

# Populate deployment settings without running commands that change the system
parse_args() {
    local home_options=false

    target_home=$HOME
    platform=auto
    hibernate=
    packages=()
    action=${1:-help}
    [[ $# -eq 0 ]] || shift

    case $action in
        -h|--help) action=help ;;
        deploy) action=install ;;
        doctor) action=status ;;
        help|check|install|status|uninstall) ;;
        *) die "Unknown action: $action" ;;
    esac

    while [[ $# -gt 0 ]]; do
        case $1 in
            --i3-hibernate|--sway-hibernate)
                [[ -z $hibernate ]] || die 'Choose one hibernation configuration.'
                hibernate=${1#--}
                shift
                ;;
            --home|--platform)
                [[ $# -ge 2 ]] || die "Missing value: $1"
                home_options=true
                if [[ $1 == --home ]]; then
                    target_home=$2
                else
                    platform=$2
                fi
                shift 2
                ;;
            --packages)
                home_options=true
                shift
                packages=("$@")
                break
                ;;
            *)
                die "Unknown option: $1"
                ;;
        esac
    done

    if [[ -n $hibernate && $action != help ]]; then
        [[ $action == install ]] ||
            die 'Hibernation options require the install command.'
        [[ $home_options == false ]] ||
            die 'Hibernation installation cannot use home deployment options.'
    fi
}

parse_args "$@"

if [[ $action == help ]]; then
    printf '%s\n' \
        'deploy.sh check|install|status|uninstall [--home PATH]' \
        '    [--platform auto|darwin|linux] [--packages NAME ...]' \
        'deploy.sh install --i3-hibernate|--sway-hibernate' \
        '    Install only Linux hibernation configuration; uses sudo'
    exit 0
fi

if [[ -n $hibernate ]]; then
    if [[ $(uname -s) != Linux ]]; then
        echo 'Hibernation setup is Linux-only.' >&2
        exit 1
    fi

    sudo rsync -rvzh "$repo_dir/$hibernate/" /
    printf '%s\n' 'Hibernate config copied. Reboot when convenient to apply it.' \
        'Do not restart systemd-logind inside your graphical session.'
    exit 0
fi

if [[ ! -d $target_home ]]; then
    echo "Target must be an existing directory: $target_home" >&2
    exit 1
fi
target_home=$(cd -- "$target_home" && pwd -P)

if [[ $platform == auto ]]; then
    case $(uname -s) in
        Darwin) platform=darwin ;;
        Linux) platform=linux ;;
    esac
fi
if [[ $platform != darwin && $platform != linux ]]; then
    echo "Unsupported platform: $platform" >&2
    exit 1
fi

# Explicit packages exclude tooling, caches and system-level hibernation
common=(
    ag emacs git kitty mise nvim ranger ripgrep scripts snippets
    tmux uctags vim yazi zsh
)
mac=(hammerspoon herdr skhd slate)
linux=(i3 mate rofi sway swaync systemd waybar wofi)
allowed=("${common[@]}" "${mac[@]}" "${linux[@]}")

if [[ ${#packages[@]} -eq 0 ]]; then
    packages=("${common[@]}")
    if [[ $platform == darwin ]]; then
        packages+=("${mac[@]}")
    else
        packages+=("${linux[@]}")
    fi
fi

for package in "${packages[@]}"; do
    valid=false
    for name in "${allowed[@]}"; do
        if [[ $package == "$name" ]]; then
            valid=true
            break
        fi
    done
    if [[ $valid != true || ! -d $repo_dir/$package ]]; then
        echo "Unknown or missing package: $package" >&2
        exit 1
    fi
done

stow_args=(
    --no-folding --verbose=1
    --dir "$repo_dir" --target "$target_home"
    '--ignore=(^|/)(node_modules|__pycache__|\.DS_Store)$'
)

if [[ $action == uninstall ]]; then
    stow "${stow_args[@]}" --simulate --delete "${packages[@]}"
    stow "${stow_args[@]}" --delete "${packages[@]}"
elif [[ $action == status ]]; then
    if ! output=$(stow "${stow_args[@]}" --simulate --stow \
        "${packages[@]}" 2>&1); then
        echo "$output" >&2
        exit 1
    fi
    # A successful simulation can still propose links missing from the target
    if [[ $output == *'LINK:'* || $output == *'MKDIR:'* ]]; then
        echo "$output"
        exit 1
    fi
    echo 'Managed links are installed.'
else
    stow "${stow_args[@]}" --simulate --restow "${packages[@]}"
    if [[ $action == check ]]; then
        echo 'Check passed; no changes made.'
        exit 0
    fi
    stow "${stow_args[@]}" --restow "${packages[@]}"
fi
