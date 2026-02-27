#!/bin/sh

# CONFIGURATION
# Matches your polybar colors
COLOR_FOCUSED_UNDERLINE="#FFFFFF"
COLOR_FOREGROUND="#B3B3B3"
COLOR_EMPTY="#4D4D4D"
COLOR_URGENT="#A5492F"

# Create format strings
# %{A1...} adds click handlers
# %{F...} sets foreground color
# %{+u}%{u...} sets underline
f_focused_free="%{+u}%{u${COLOR_FOCUSED_UNDERLINE}}%{F${COLOR_EMPTY}}"
f_focused_occupied="%{+u}%{u${COLOR_FOCUSED_UNDERLINE}}%{F${COLOR_FOREGROUND}}"
f_occupied="%{F${COLOR_FOREGROUND}}"
f_free="%{F${COLOR_EMPTY}}"
f_urgent="%{B${COLOR_URGENT}}%{F#FFFFFF}"
reset="%{F-}%{-u}%{B-}"

# Listen to bspwm changes
bspc subscribe report | while read -r line; do
    output=""

    # Split on ':' by temporarily changing IFS
    oldIFS="$IFS"
    IFS=':'
    # shellcheck disable=SC2086
    set -- $line
    IFS="$oldIFS"

    # Iterate through the report items
    for item in "$@"; do
        # Extract first character (state) and remainder (name)
        state=$(printf '%s' "$item" | cut -c1)
        name=$(printf '%s' "$item" | cut -c2-)

        # Skip layout (L) or monitor (M/m) flags if necessary,
        # typically desktops start with o/f/u/O/F/U
        case "$state" in
            f) # free (empty), inactive
                output="$output%{A1:bspc desktop -f $name:} ${f_free} $name ${reset} %{A}"
                ;;
            F) # free (empty), FOCUSED -> DARK GREY TEXT + WHITE UNDERLINE
                output="$output%{A1:bspc desktop -f $name:} ${f_focused_free} $name ${reset} %{A}"
                ;;
            o) # occupied, inactive
                output="$output%{A1:bspc desktop -f $name:} ${f_occupied} $name ${reset} %{A}"
                ;;
            O) # occupied, FOCUSED -> LIGHT GREY TEXT + WHITE UNDERLINE
                output="$output%{A1:bspc desktop -f $name:} ${f_focused_occupied} $name ${reset} %{A}"
                ;;
            u|U) # urgent
                output="$output%{A1:bspc desktop -f $name:} ${f_urgent} $name ${reset} %{A}"
                ;;
        esac
    done
    printf '%s\n' "$output"
done