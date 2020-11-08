#!/usr/bin/env bash

if ! command -v emacs 1>/dev/null
then
    sudo apt install emacs -y
else
    echo "emacs installed"
fi

emacs -Q --script start.el
