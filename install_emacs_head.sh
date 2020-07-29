#!/usr/bin/env bash

  
brew tap daviderestivo/emacs-head  
brew install emacs-head@28 --with-cocoa --with-imagemagick --with-jansson --with-pdumper --with-xwidgets

ln -s /usr/local/opt/emacs-head@28/Emacs.app /Applications

brew services start daviderestivo/emacs-head/emacs-head@28



