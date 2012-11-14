#!/bin/bash
# Thanks to Buck Ryan (github.com/b-ryan) for the template for this script
#
# Prereqs:
# - git, zsh, emacs, curl

LINK_FLAGS="-s"
if [ "$1" = "--force" ]
then
    LINK_FLAGS="-sf"
fi
DIR_LINK_FLAGS="${LINK_FLAGS}n"

# arguments:
# 1 flags to the ln command
# 2 the local file in this directory that will be linked
# 3 (optional) the destination relative to the home directory
#   for example, instead of /home/user/.vim, this arguments would
#   just be .vim. If no argument given, the destination will be
#   the same as the source, but prepended with a dot.
create_link() {
    FLAGS=$1
    SOURCE_REL=$2
    SOURCE_ABS=`pwd`/$2
    LINK=$3
    [ $LINK ] || LINK=.$SOURCE_REL
    ln $FLAGS $SOURCE_ABS ~/$LINK && echo "created link to '$SOURCE_ABS'"
}
symlink() {
    create_link $LINK_FLAGS $1 $2
}
dirlink() {
    create_link $DIR_LINK_FLAGS $1 $2
}

dirlink emacs
symlink .emacs
symlink .zshrc
symlink .gitconfig

# Install oh-my-zsh
curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | sh

true