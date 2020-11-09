#!/usr/bin/env bash


if ! command -v emacs 1>/dev/null
then
    echo "emacs not found, installing"
    sudo apt install emacs -y
else
    echo "emacs installed"
fi

SCRIPT_PATH=$(dirname "$0")

emacs -Q --script ${SCRIPT_PATH}/start.el
