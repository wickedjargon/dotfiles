if status is-interactive
    # Disable inline autosuggestions
    set -g fish_autosuggestion_enabled 0

    # Disable syntax highlighting (Comprehensive)
    set -g fish_color_normal normal
    set -g fish_color_command normal
    set -g fish_color_quote normal
    set -g fish_color_redirection normal
    set -g fish_color_end normal
    set -g fish_color_error normal
    set -g fish_color_param normal
    set -g fish_color_comment normal
    set -g fish_color_match normal
    set -g fish_color_selection --reverse
    set -g fish_color_search_match --reverse
    set -g fish_color_operator normal
    set -g fish_color_escape normal
    set -g fish_color_autosuggestion normal
    
    # Additional variables to ensure no underlining
    set -g fish_color_valid_path normal
    set -g fish_color_cwd normal
    set -g fish_color_cwd_root normal
    set -g fish_color_user normal
    set -g fish_color_host normal
    set -g fish_color_host_remote normal
    set -g fish_color_cancel normal
    set -g fish_color_keyword normal
    set -g fish_color_option normal

    # Aliases
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias z='zathura'
    alias em='emacs'
    alias emc='emacsclient -c'
    alias pi-system='qemu-system-arm -kernel ~/.qemu-files/kernel-qemu-4.4.34-jessie -cpu arm1176 -m 256 -M versatilepb -serial stdio -append "root=/dev/sda2 rootfstype=ext4 rw" -hda ~/.qemu-files/2017-04-10-raspbian-jessie.img -nic user,hostfwd=tcp::5022-:22 -no-reboot'
    alias pi-ssh='ssh pi@127.0.0.1 -p 5022'
    alias pi-sshfs='sshfs pi@127.0.0.1:/home/pi/projects ~/d/projects/remote-projects/ -p 5022'
    alias r="rlwrap"
    alias android-emulator="~/Android/Sdk/emulator/emulator -avd Medium_Phone_API_36 -snapshot default_boot"
    alias neofetch="fastfetch"

    # Environment Variables
    set -gx EDITOR vis
    set -gx BROWSER firefox
    set -gx SUDO_EDITOR vis
    set -gx OCEN_ROOT $HOME/.local/src/ocen
    set -gx JAVA_HOME /usr/lib/jvm/default
    set -gx ZED_ALLOW_EMULATED_GPU 1
    
    # QT Theme
    set -gx QT_QPA_PLATFORMTHEME qt5ct
    set -gx QT_QPA_PLATFORMTHEME_QT6 qt6ct

    # Prompt
    function fish_prompt
        set_color -o green
        echo -n "$USER@$hostname"
        set_color normal
        echo -n ":"
        set_color -o blue
        echo -n (prompt_pwd)
        set_color normal
        echo
        echo -n '$ '
    end

    # PATH (appending to preserve order from bashrc loop)
    # Bash loop: export PATH=$PATH:$apath
    fish_add_path -a /usr/sbin
    fish_add_path -a /sbin
    fish_add_path -a $HOME/.local/bin
    fish_add_path -a $HOME/.cargo/bin
    fish_add_path -a /usr/local/go/bin
    fish_add_path -a $HOME/go/bin
    fish_add_path -a $HOME/.ghcup/bin
    fish_add_path -a $OCEN_ROOT/bin
    fish_add_path -a $OCEN_ROOT/bootstrap
    fish_add_path -a $HOME/.local/src/v
    fish_add_path -a $HOME/.asdf/shims
    fish_add_path -a $HOME/.dotnet/tools
    fish_add_path -a $JAVA_HOME/bin

    # Secrets
    if test -f ~/.secrets
        source ~/.secrets
    end
end
