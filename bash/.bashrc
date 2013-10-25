#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR="vim"
alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
complete -cf sudo
complete -cf man
