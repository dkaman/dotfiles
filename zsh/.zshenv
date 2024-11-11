#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

export PATH="/home/wzrd/.local/bin:$PATH:/usr/local/go/bin:$HOME/go/bin:$HOME/tools/bin"
export GOPATH="$HOME/go"

export USER_BOOKMARKS=/home/wzrd/.shell_bookmarks
export SYS_BOOKMARKS=/etc/shell_bookmarks

export EDITOR="emacsclient -a emacs "
export PAGER=less

export PATH=$PATH:$HOME/.cargo/bin
