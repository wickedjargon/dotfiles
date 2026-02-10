# exports
export EDITOR="vis"
export BROWSER="firefox"
export SUDO_EDITOR="vis"
export OCEN_ROOT=$HOME/.local/src/ocen
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
export ZED_ALLOW_EMULATED_GPU=1

# qt theme
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_QPA_PLATFORMTHEME_QT6=qt6ct

# PATH env variable
paths=(
  "/usr/sbin"
  "/sbin"
  "$HOME/.local/bin"
  "$HOME/.cargo/bin"
  "/usr/local/go/bin"
  "$HOME/go/bin"
  "$HOME/.ghcup/bin"
  "$OCEN_ROOT/bin"
  "$OCEN_ROOT/bootstrap"
  "$HOME/.local/src/v"
  "$HOME/.asdf/shims"
  "$HOME/.dotnet/tools"
  "$JAVA_HOME/bin"
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
alias emc='emacsclient -c'
alias pi-system='qemu-system-arm -kernel ~/.qemu-files/kernel-qemu-4.4.34-jessie -cpu arm1176 -m 256 -M versatilepb -serial  stdio -append "root=/dev/sda2 rootfstype=ext4 rw" -hda ~/.qemu-files/2017-04-10-raspbian-jessie.img  -nic user,hostfwd=tcp::5022-:22 -no-reboot'
alias pi-ssh='ssh pi@127.0.0.1 -p 5022'
alias pi-sshfs='sshfs pi@127.0.0.1:/home/pi/projects ~/d/projects/remote-projects/ -p 5022'
alias r="rlwrap"
alias android-emulator="~/Android/Sdk/emulator/emulator -avd Medium_Phone_API_36 -snapshot default_boot"
alias neofetch="fastfetch"

# bind ctrl+l to cls (using macro to force full prompt redraw)
bind '"\C-l": " \C-a\C-kcls\n\C-y"'

# add color and newline before prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n\$ '

# auto complete with tab
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion


if [ -f ~/.secrets ] ; then source ~/.secrets ; fi
