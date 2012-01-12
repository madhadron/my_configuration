[[ $PS1 && -f $CONFIG_DIR/bash_completion ]] && \
    source $CONFIG_DIR/bash_completion

[[ $PS1 && -f $CONFIG_DIR/git-completion ]] && \
    source $CONFIG_DIR/git-completion

PS1='\h:\W$(__git_ps1 "(%s)") \u\$ '

alias rhino="ssh fross@rhino"
alias meph="ssh fross@mephistopheles"
alias molmicro="ssh fredross@molmicro"
alias sci="ssh labmed+fredross@sci.labmed.washington.edu"

export CDPATH=.:~:~/data:~/data/writing:~/data/projects:~/data/code
export HISTIGNORE="&:ls:ls *:emacs:[bf]g:exit"
set -o ignoreeof

shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s extglob

if [ 'rhino' \< `hostname` -o 'hyrax' \< `hostname` ]; then
    umask 007
    export MATSENGRP=/mnt/orca/home/phs_grp/matsengrp
    export PATH="$MATSENGRP/local/bin:$PATH"
    export LD_LIBRARY_PATH="$MATSENGRP/local/lib:$LD_LIBRARY_PATH"
    export SILO="/shared/silo_researcher/Matsen_F/MatsenGrp/"
    export WORKING="$SILO/working/fross"
    export CDPATH="$CDPATH:$SILO:$WORKING:$SILO/micro_refset"
    alias wd="cd $WORKING"
    alias silo="cd $SILO"
fi

if [ `uname` = 'Linux' ]; then
    # Generic Linux configuration
    :
fi

if [ `uname` = 'Darwin' ]; then
    # Laptop configuration
    :
fi

