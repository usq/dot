#!/usr/bin/env bash

PATH="$PATH:${HOME}/bin"
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='emacsclient -nw'
fi

# e vim editing mode in terminal [escape to enter normal mode]
bindkey -v
fpath+=("$HOME/.zsh/pure")
# ake Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)

export TERM="xterm-256color"
export KEYTIMEOUT=1


# .zshrc
# pure prompt
autoload -U promptinit; promptinit
prompt pure


# Restore some keymaps removed by vim keybind mode
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^e' forward-char
bindkey "^r" history-incremental-search-backward

# Dependencies for the following lines
zmodload zsh/zle
autoload -U colors && colors

# Change prompt icon + color based on insert/normal vim mode in prompt
# Will have no effect if you don't use pure as your ZSH theme
export PURE_PROMPT_SYMBOL="[I] ❯"
export PURE_PROMPT_VICMD_SYMBOL="%{$fg[green]%}[N] ❮%{$reset_color%}"

# By default, we have insert mode shown on right hand side
export RPROMPT="%{$fg[blue]%}[INSERT]%{$reset_color%}"

# And also a beam as the cursor
# echo -ne '\e[5 q'

# Callback for vim mode change
function zle-keymap-select () {
    # Only supported in these terminals
    if [ "$TERM" = "xterm-256color" ] || [ "$TERM" = "xterm-kitty" ] || [ "$TERM" = "screen-256color" ]; then
        if [ $KEYMAP = vicmd ]; then
            # Command mode

            export RPROMPT="%{$fg[green]%}[NORMAL]%{$reset_color%}"

#            echo -ne '\e[1 q'
#            printf %b "\e]12;$fg[green]\a"
            # Set block cursor
            # printf %b '\e]12;green\a'
        else
            # Insert mode

            export RPROMPT="%{$fg[blue]%}[INSERT]%{$reset_color%}"

            # Set beam cursor
 #           echo -ne '\e[5 q'
        fi
    fi

    if typeset -f prompt_pure_update_vim_prompt_widget > /dev/null; then
        # Refresh prompt and call Pure super function
        prompt_pure_update_vim_prompt_widget


    fi
}

# Bind the callback
zle -N zle-keymap-select
