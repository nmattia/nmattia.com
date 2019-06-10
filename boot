#!/usr/bin/env bash
# vim: set ft=bash

set -x
set -euo pipefail

run_root() {
  if [[ $EUID -ne 0 ]]; then
    str="$@"
    sudo "$str"
  else
    str="$@"
    bash -c "$str"
  fi
}

run_user() {
  if [[ $EUID -ne 0 ]]; then
    str="$@"
    bash -c "$str"
  else
    str="$@"
    su - nicolas -c "$str"
  fi
}

if [ "$(grep -Ei 'debian|buntu|mint' /etc/*release)" ]
then
    echo "Debian system, upgrading"
    run_root apt update
    run_root apt upgrade -y
    run_root apt install mosh -y
else
    echo "Not Debian system, not upgrading"
fi

if id -u nicolas
then
    echo "User nicolas already exists"
else
    echo "User nicolas doesn't exist, creating"
    run_root adduser nicolas \
        --gecos "First Last,RoomNumber,WorkPhone,HomePhone" \
        --disabled-password
fi

if [ -d /nix ]
then
    echo "Nix dir exists"
else
    echo "Nix dir doesn't exist, installing Nix"
    run_root mkdir -m 0755 /nix
    run_root chown nicolas /nix
    run_user 'curl https://nixos.org/nix/install | sh'
    run_user "echo '. ~/.nix-profile/etc/profile.d/nix.sh' >> ~/.bashrc"
    run_root mkdir -p /etc/nix
    run_root 'echo "sandbox = true" > /etc/nix/nix.conf'
fi


echo "Installing homies"
run_user nix-env -if \
    https://github.com/nmattia/homies/tarball/master?dummy=$(date | sha256sum | head -c 20) \
    --remove-all --max-jobs 20
run_user "echo 'if [ -x "'"$(command -v bashrc)"'" ]; then "'$(bashrc)'"; fi' >> ~/.bashrc"
