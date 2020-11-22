#!/usr/bin/env bash

#set -e

# function spacemacspull() {
# 	  pushd ~/.emacs.d
# 	  git pull
# 	  popd
# }


# unset -f tmux >/dev/null 2>&1
# real_tmux=`which tmux`
# function ttmux {
#     if [ $# -eq 0 ] || [[ "$@" =~ ^a.* ]] && [[ ! "$@" =~ ^a.*[[:space:]]-t[[:space:]] ]] # hijack non-specific 'tmux' or tmux attach 
#     then
#         if $real_tmux ls >/dev/null 2>&1 # if there are existing tmux sessions
#         then
#             # determine whether to attach (from outside tmux) or switch to a different session (already within tmux)
#             if [ -z $TMUX ] # if $TMUX is empty no session is attached
#             then
#                 attachswitch=attach
#             else
#                 attachswitch=switch
#             fi

#             # prompt to select existing tmux session then attach
#             # $real_tmux $attachswitch -t `$real_tmux list-sessions | fzf-tmux | cut -d ":" -f1`
#             new_session_option='New session'
#             session_choice=`$real_tmux list-sessions | sed '$a'"$new_session_option" | ~/.fzf/bin/fzf-tmux | cut -d ":" -f1`
#             # printf "$session_choice"
#             if [[ $session_choice == $new_session_option ]]
#             then
#                 echo "New session selected."
#                 if [ -z $TMUX ]
#                 then
#                     $real_tmux new-session
#                 else
#                     # Already in a tmux session. Replace current shell with new one to avoid nesting.
#                     exec $real_tmux new-session
#                 fi
#             else
#                 if [ ! -z $session_choice ]
#                 then
#                     echo "Attaching to session $session_choice"
#                     $real_tmux $attachswitch -t $session_choice 2>/dev/null
#                 else
#                     echo "No selection"
#                     # exit 1
#                     return 1
#                     # if [ -z $TMUX ]
#                     # then
#                     #     $real_tmux kill-session
#                     # fi
#                 fi
#             fi
#         else # no existing tmux sessions
#             waitsecs=5
#             echo -n "No existing tmux sessions. Starting a new one in $waitsecs seconds. ENTER to skip. CTRL+C to cancel."
#             # sleep $waitsecs
#             read -t $waitsecs; printf "\rStarting new tmux session now.\n"
#             $real_tmux new-session
#         fi
#     else
#         # don't hijack this command - run it as normal
#         $real_tmux "$@"
#     fi
# }

# Start TMUX automatically
## end tmux session chooser

# fbr - checkout git branch (including remote branches)
fbr() {
    local branches branch
    branches=$(git branch --all | grep -v HEAD) &&
        branch=$(echo "$branches" |
                     fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
        git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

