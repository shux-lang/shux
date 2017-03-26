#!/bin/bash

# Automates the OCaml installation described in:
# https://github.com/realworldocaml/book/wiki/Installation-Instructions
# ...plus some project-specific libraries.

# Safer shell scripting: https://sipb.mit.edu/doc/safe-shell/
set -euf -o pipefail

command_exists () {
    hash "$1" 2> /dev/null;
}

choice () {
    while true; do
        read -p "$1 [y/n] " yn
        case $yn in
            [Yy]* ) return 0;;
            [Nn]* ) return 1;;
            * ) echo 'Please answer yes or no.';;
        esac
    done
}

die () {
    echo "$1"
    exit 1
}

platform='unknown'
uname_str=`uname`
if [[ "$uname_str" == 'Linux' ]]; then
    platform='linux'
    linux_distro=`lsb_release -is`
    if [[ "$linux_distro" == 'Ubuntu' ]]; then
        platform='ubuntu'
    elif [[ "$linux_distro" == 'Debian' ]]; then
        platform='debian'
    else
        echo -ne 'Unsupported Linux distro: '
        lsb_release -is
        die 'Sorry :('
    fi
elif [[ "$uname_str" == 'Darwin' ]]; then
    platform='osx'
else
    die 'Unsupported platform. Sorry :('
fi

echo 'Downloading submodules'
git submodule update --init

echo 'Installing OPAM, OCaml, and STK'

opam_extra_args=""

if [[ "$platform" == 'ubuntu' || "$platform" == 'debian' ]]; then
    sudo apt-get update
    if ! apt-cache show opam > /dev/null; then
        echo 'Your Ubuntu version is too old. Adding ppa:avsm/ppa to get OPAM.'
        if ! command_exists 'add-apt-repository'; then
            sudo apt-get install python-software-properties
        fi
        sudo add-apt-repository ppa:avsm/ppa
        sudo apt-get update
    fi
    sudo apt-get install m4 ocaml-native-compilers camlp4-extra aspcud opam \
    stk stk-doc libstk0-dev --no-install-recommends
    
    # Ubuntu's OCaml package is ridiculously old. Ask OPAM to compile v4.02.3 for us.
    opam_extra_args="--comp 4.02.3"
elif [[ "$platform" == 'osx' ]]; then
    # Check for Xcode
    if ! command_exists 'xcode-select'; then
        die 'Please install Xcode from the Mac App Store.'
    fi
    
    # Check if command line tools installed
    if ! xcode-select -p > /dev/null; then
        xcode-select --install
        die 'Please install the Xcode command line tools and run this script again.'
    fi
    
    if command_exists 'brew'; then
        echo 'Installing packages with Homebrew.'
        brew install opam stk
    elif command_exists 'port'; then
        echo 'Installing packages with MacPorts.'
        sudo port install opam stk
    else
        die 'Please install Homebrew or MacPorts to get OPAM.'
    fi
fi

if choice 'Have you installed llvm-3.6-dev on Ubuntu?'; then
    yes | echo 'Great, continuing with opam setup...' || true
else
    die 'Please install before attempting to run the script again.'
fi

# Configure OPAM
if [[ -x "$HOME/.opam/" ]]; then
    echo 'opam init has already run. Skipping.'
else
    echo 'By default, OPAM modifies your .profile, .ocamlinit, and auto-complete scripts.'
    if choice 'Install OPAM with the default settings?'; then
        # Answer 'y' to all of OPAM's questions
        # For some reason, opam init doesn't return 0 on success.
        yes | opam init $opam_extra_args || true
    else
        # Let the user deal with OPAM
        opam init $opam_extra_args || true
    fi
fi

# Add OPAM stuff to current session
eval `opam config env`

echo 'Installing package dependencies'
opam install core utop ocamlfind "llvm=3.6"

echo 'Please add the following to ~/.ocamlinit:'
echo ''
echo \
'#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;'
echo ''

echo 'Done!'
