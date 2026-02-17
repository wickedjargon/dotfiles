# exports
export EDITOR="vis"
export SUDO_EDITOR="vis"
export BROWSER="firefox"

# qt theme
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_QPA_PLATFORMTHEME_QT6=qt6ct

# Android / Java
export ANDROID_HOME="$HOME/Android/Sdk"
export JAVA_HOME="/usr/lib/jvm/java-21-openjdk"

# PATH env variable
paths=(
  "/usr/sbin"
  "/sbin"
  "$HOME/.local/bin"
  "$HOME/.cargo/bin"
  "$ANDROID_HOME/cmdline-tools/latest/bin"
  "$ANDROID_HOME/platform-tools"
)

for apath in "${paths[@]}"; do
    export PATH=$PATH:$apath
done

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias z='zathura'
alias em='emacs'
alias emc='emacsclient -c -a ""'
alias neofetch="fastfetch"

# bind ctrl+l to cls (using macro to force full prompt redraw)
bind '"\C-l": " \C-a\C-kcls\n\C-y"'

# add color and newline before prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n\$ '

# auto complete with tab
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion
