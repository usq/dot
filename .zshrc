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

plugins=(git docker git-extras history jsontools vi-mode
        # npm
        # osx
        # tmux fasd
       #  zsh-syntax-highlighting zsh-autosuggestions
        )
# vi-mode vi-mode-agnoster
#

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8
export LC_ALL=$LANG

export PATH="/usr/local/sbin:$PATH:${HOME}/bin:${HOME}/.cargo/bin:${HOME}/.emacs.d/bin"
export ALTERNATE_EDITOR="vim"


fpath+=("$HOME/.zsh/pure")
autoload -U promptinit; promptinit
prompt pure


[ -f ~/.config/usq/functions/fn.sh ] && source ~/.config/usq/functions/fn.sh
[ -f ~/.config/usq/alias/al.sh ] && source ~/.config/usq/alias/al.sh
[ -f ~/.config/usq/msc/msc.sh ] && source ~/.config/usq/msc/msc.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Will return non-zero status if the current directory is not managed by git
is_in_git_repo() {
    git rev-parse HEAD > /dev/null 2>&1
}

gt() {
    # "Nothing to see here, move along"
    is_in_git_repo || return
    git branch -a | fzf-tmux --multi
}

# A helper function to join multi-line output from fzf
join-lines() {
    local item
    while read item; do
        echo -n "${(q)item} "
    done
}

CAPS_LOCK() {
    xdotool key Caps_Lock
}


psg() {
  ps aux | grep "$@"
}

git-remote-branches() {
    git for-each-ref --format='%(committerdate) %02 %(authorname) %09 %(refname:lstrip=2)' --sort=committerdate refs/remotes
}


fzf-gt-widget() LBUFFER+=$(gt | join-lines)
zle -N fzf-gt-widget
bindkey '^B' fzf-gt-widget

alias cat=bat

# convenience
# use markdown files with emacs
alias -s md=ec

# make named directory
#hash -d W=~/dev/werkstatt

export spawn=~/dev/lab/spawn

bindkey -v
