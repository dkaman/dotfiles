#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

source ~/.zshenv

alias e=$EDITOR
alias -g noerr="2>/dev/null"
alias binstrip="tr -cd '\11\12\15\40-\176' < "
alias rsync="rsync -avz --progress"
alias open="xdg-open"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Source the fzf keybindings file
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Shell bookmarking :)
prune_bookmarks() {
    if [ -f $USER_BOOKMARKS ]; then
        for bookmark in $(cat $USER_BOOKMARKS); do
            if [ ! -d $bookmark ]; then
                # The variable we're going to pass to sed will contain slashes
                # since they are paths, we need to use an alternate delimeter for
                # our regex, in this case '#'
                sed -i'' "\#$bookmark#d" $USER_BOOKMARKS
            fi
        done
    fi
}

cdscuts_list_echo() {
    # Removes lines that start with '#' and lines that are just whitespace
    cat $1 | sed 's/#.*//g' | sed '/^\s*$/d'
}

cdscuts_glob_echo() {
    # Variables to hold file lists
    local system_wide_filelist=''
    local user_filelist=''

    # If system-wide file exists, get all entries (ignoring blank lines and comments marked with '#')
    if [ -r $SYS_BOOKMARKS ]; then
       system_wide_filelist=$(cdscuts_list_echo $SYS_BOOKMARKS)
    fi

    # If user file exists, get all entries (ignoring blank lines and comments marked with '#')
    if [ -r $USER_BOOKMARKS ]; then
       user_filelist=$(cdscuts_list_echo $USER_BOOKMARKS)
    fi

    echo -e "$system_wide_filelist\n$user_filelist" | sed '/^\s*$/d'
}

fd() {
   # Before we do anything, clear out the stale bookmarks
   prune_bookmarks

   # Get all of the entries in the system and user files
   file_list=$(cdscuts_glob_echo)

   if [ -z $TMUX ]; then
       local executable="fzf"
   else
       local executable="fzf-tmux"
   fi

   # use FZF to select from the list
   local dest_dir=$(echo -e $file_list | $executable)

   # If the result isn't the empty string, cd into it
   if [[ $dest_dir != '' ]]; then
      cd "$dest_dir"
   fi
}

bm() {
  if [ -z "$1" ]; then
      echo $(pwd) >> $USER_BOOKMARKS
  else
      echo "$1" >> $USER_BOOKMARKS
  fi
}

dired() {
    if [ -z $1 ]; then
        emacsclient -e "(dired \"$(pwd)\")"
    else
        emacsclient -e "(dired \"$1\")"
    fi
}

[ -f "/home/wzrd/.ghcup/env" ] && . "/home/wzrd/.ghcup/env" # ghcup-env