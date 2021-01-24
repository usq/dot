# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"
ZSH_THEME=""

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

plugins=(git docker git-extras history osx tmux fasd zsh-syntax-highlighting zsh-autosuggestions)
# vi-mode vi-mode-agnoster
#

source $ZSH/oh-my-zsh.sh

#export LANG=en_US.UTF-8
#export LC_ALL=$LANG

#export PATH="/usr/local/sbin:$PATH:${HOME}/bin:${HOME}/.cargo/bin:${HOME}/.emacs.d/bin"
#export ALTERNATE_EDITOR="vim"


#Warning: Homebrew's sbin was not found in your PATH but you have installed
#formulae that put executables in /usr/local/sbin.
# ls /usr/local/sbin:
# comsatd iftop imap4d lmtpd mda nethogs pop3d unbound unbound-anchor unbound-checkconf
# unbound-control unbound-control-setup unbound-host
export PATH="/usr/local/sbin:$PATH"

# Checks if a passed file exists and sources it
#
# Args: $1: file to source if it exists
function __source_if_found()
{
    [ -f "${1}" ] && source "${1}"
}

files=(
    ~/.config/usq/alias/al.sh
    ~/.config/usq/functions/fn.sh
    ~/.config/usq/msc/msc.sh # initializes pure prompt
    ~/.fzf.zsh
    /usr/share/doc/fzf/examples/key-bindings.zsh
    /usr/share/doc/fzf/examples/completion.zsh
    ~/.config/broot/launcher/bash/br
)

for f in ${files[@]}; do
    __source_if_found "${f}"
done

psg() {
  ps aux | grep "$@"
}
