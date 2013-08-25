# load out own functions and extensions
fpath=(~/.zsh $fpath)
# this is the modules section
# step 1: setup dependencies
zmodload -d zsh/zftp zsh/net/tcp
# step 2: setup autoloading to keep the environment clean
zmodload -ab zsh/zftp zftp
# step 3: load a few modules directly
#zmodload zsh/cap
zmodload -i zsh/pcre                # the pcre module defines an expression, therefor we load it manually

# load a few funky extensions
autoload -U compinit promptinit zcalc zfinit
autoload -Uz vcs_info

# initialize a few things
compinit   # completion
promptinit # fancy prompts
zfinit     # FTP !!!

# choose a funky prompt
prompt bsx # default gentoo: green for users, red for root
#prompt bart # prompt in two lines which keeps track of what you did and when

# enable some funky completion options
#  colors for completion
zstyle ':completion:*' list-colors ''
#  caching of completions so the functions don't have to be rerun each time
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zshcache
#  ignore CVS directories in completion
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS' '(|*/).svn'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS' '(*/)#.svn'
#  approximate matching, tollerate misspeled commands
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
#   the more you type, the more errors are allowed
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
#  ignore completion functions for unavailable commands
zstyle ':completion:*:functions' ignored-patterns '_*'
#  complete PIDs for kill through a menu
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:kill:*'   command 'ps -fHu$USER'
#  complete PIDs for kill through a menu
zstyle ':completion:*:*:jstat:*' menu yes select
zstyle ':completion:*:jstat:*'   force-list always
zstyle ':completion:*:jstat:*'   command 'jps'
#  remove slashes at the end of completed directories
zstyle ':completion:*' squeeze-slashes true

autoload -U zsh/terminfo
# force emacs line editing mode
bindkey -e
bindkey $terminfo[khome] beginning-of-line
bindkey $terminfo[kend]  end-of-line
bindkey $terminfo[kdch1] delete-char
bindkey $terminfo[kpp] history-beginning-search-backward
bindkey $terminfo[knp] history-beginning-search-forward

# kewl options
setopt autocd     # allow '..' instead of 'cd ..' without aliases
setopt correct    # turn on spelling correction through the completion mechanism
setopt correctall # examine every word on the commandline

# useful aliases
alias ls='ls --color=auto'           # see all the beautiful colors
alias l='ls -l'                      # i'm lazy
alias ll='ls -Al'                    # type more, see more
alias cpi='rsync --verbose --progress' # interactive copying of files
alias grep='grep --color=auto'
alias mkdir='nocorrect noglob mkdir' # spelling correction makes no sense for nonexistent stuff
alias uu="sudo su -"                 # real root shell
alias plagueis='ssh -t plagueis "screen -U -x"'
alias vader='ssh -t vader "screen -U -x"'
alias brezn='ssh -t brezn.muc.ccc.de "screen -U -x"'
alias ns2="TERM=xterm ssh -t ns2"
alias cookie='fortune .. off'
alias psu='ps -fHu$USER'             # show all MY processes in a tree
alias psa='ps -fHe'                  # show ALL processes in a tree
alias truecrypt='truecrypt -t --mount-options=timestamp'
alias tcmount='truecrypt -t --mount-options=timestamp -k "" --protect-hidden=no'
alias cal='cal -m'
alias 3m='cal -m3'
alias year='cal -my'
alias services='netstat -tulpn'
alias goa='mplayer -playlist "http://yp.shoutcast.com/sbin/tunein-station.pls?id=5628"'

HOSTNAME=`hostname`
# include config specific to the hostname if it exists
if [[ -f ~/.zsh/$HOSTNAME.zshrc ]]; then
    source ~/.zsh/$HOSTNAME.zshrc
fi

# start ssh and gpg agents if available
if [[ -f ~/.startagents ]]; then
    . ~/.startagents
fi

# python virtualenvs
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/projects
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true
export VIRTUALENV_USE_DISTRIBUTE=true
[[ -e /usr/bin/virtualenvwrapper.sh ]] && source /usr/bin/virtualenvwrapper.sh
# if we are in a vritualenv cd automatically goes to the root of the working directory
cd () {
    if (( $# == 0 ))
    then
        builtin cd $WORK_DIR
    else
        builtin cd "$@"
    fi
}

# this function is run before a command is executed
preexec() {
    # check for missing packages for failed commands
    # (c) Zygmunt Krynicki 2007 (GPL)
    # command="${1%% *}"
}

# this function is executed before the command prompt is displayed
precmd() {
    # check for missing packages for failed commands
    # (c) Zygmunt Krynicki 2007 (GPL)
    #(($?)) && [ -n "$command" ] && {
    #    whence -- "$command" >& /dev/null ||
    #        python /usr/lib/command-not-found -- "$command"
    #    unset command
    #}
    # make the terminal aware of our current location
    psvar=()
    vcs_info
    [[ -n $vcs_info_msg_0_ ]] && psvar[1]="$vcs_info_msg_0_"
    [[ -t 1 ]] || return
    case $TERM in
        *xterm*|rxvt*|(dt|k|E)term)
            print -Pn "\e]2;%n@%m:%~\a"
            ;;
        screen)
            print -Pn "\e_%n@%m:%~\e\\"
            ;;
    esac
}

# this function is called whenever the working directory is changed
chpwd() {
    # see if there are any TODOs for that directory/project
    #if [ -e .venv ]; then
    #    workon `cat .venv`
    #fi
}

# named directories
#hash -d mydir=/path/to/my/dir
#hash -d subdir=~mydir/subdir

# more cool options
READNULLCMD=less     # read files through 'less' by typing '< filename'

# History setup, write every line straight to the history file and share it with other instances
setopt HistIgnoreAllDups HistIgnoreSpace ExtendedHistory IncAppendHistory ShareHistory HistSaveNoDups HistVerify HistReduceBlanks
HISTSIZE=12000
SAVEHIST=10000
HISTFILE=$HOME/.zshhist

# cool functions
namedir() { hash -d $1=$PWD ;  : ~$1 }

reload() { zcompile ~/.zshrc && source ~/.zshrc }

recordWindow() {
    local windowID
    print "Click into the window you would like to record"
    windowID=`xwininfo |grep "Window id:"|sed -e "s/xwininfo\:\ Window id:\ // ;s/\ .*//"`
    print "Recording window $windowID"
    recordmydesktop --no-sound -windowid $windowID --zero-compression
}

extract_archive () {
    local lower
    lower=${(L)1}
    if [[ $lower == *.tar.gz || $lower == *.tgz || $lower == *.tar.bz2 || $lower == *.tbz || $lower == *.tar ]]; then
        tar xf $1
    elif [[ $lower == *.jar || $lower == *.war ]]; then
        jar xf $1
    elif [[ $lower == *.zip ]]; then
        unzip $1
    elif [[ $lower == *.rar ]]; then
        unrar x $1
    else
        print "Unknown archive type: $1"
        return 1
    fi
}

list_archive () {
    local lower
    lower=${(L)1}
    if [[ $lower == *.tar.gz || $lower == *.tgz || $lower == *.tar.bz2 || $lower == *.tbz || $lower == *.tar ]]; then
        tar tvf $1
    elif [[ $lower == *.jar || $lower == *.war ]]; then
        jar tvf $1
    elif [[ $lower == *.zip ]]; then
        unzip -l $1
    elif [[ $lower == *.rar ]]; then
        unrar l $1
    else
        print "Unknown archive type: $1"
        return 1
    fi
}

_vmid() {
    # TODO
}

alias x=extract_archive # eXtract
alias p=list_archive    # Peek

source ~/.zsh/*.completion
