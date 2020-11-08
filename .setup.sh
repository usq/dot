#!/usr/bin/env bash
# set -e

# # setup
# # https://www.anand-iyer.com/blog/2018/a-simpler-way-to-manage-your-dotfiles.html
# # dotfiles config --local status.showUntrackedFiles no


# # setup new machine
# # git clone --separate-git-dir=$HOME/.dotfiles https://github.com/usq/dot-linux.git tmpdotfiles
# # rsync --recursive --verbose --exclude '.git' tmpdotfiles/ $HOME/
# # rm -r tmpdotfiles

# # or just:
# # git clone --separate-git-dir=$HOME/.dotfiles https://github.com/usq/dot-linux.git ~
# if [ ! -d "${HOME}/.dotfiles" ]; then
# 	git clone --separate-git-dir=$HOME/.dotfiles git@github.com/usq/dot-linux.git tmpdotfiles
# 	rsync --recursive --verbose --exclude '.git' tmpdotfiles/ $HOME/
# 	#rm -r tmpdotfiles
# fi

# # dotfiles config --local status.showUntrackedFiles no
# # git setup
# git config --global alias.co checkout
# git config --global alias.br branch
# git config --global alias.cm commit
# git config --global alias.st status
# git config --global alias.unstage 'reset HEAD --'

# # repeat rate
# # xset r rate 200 40

# # vim plug
# curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
#     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# # mkdir -p "$HOME/.zsh"
# # git clone https://github.com/sindresorhus/pure.git "$HOME/.zsh/pure"
# # fpath+=("$HOME/.zsh/pure")
