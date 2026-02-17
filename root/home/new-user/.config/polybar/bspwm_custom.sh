#!/bin/bash

# CONFIGURATION
# Matches your polybar colors
COLOR_FOCUSED_UNDERLINE="#5294e2"
COLOR_FOREGROUND="#5294e2"
COLOR_EMPTY="#5c616c"
COLOR_URGENT="#CF6A4C"

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
    IFS=':' read -r -a items <<< "$line"
    
    # Iterate through the report string
    for item in "${items[@]}"; do
        state="${item:0:1}"
        name="${item:1}"

        # Skip layout (L) or monitor (M/m) flags if necessary, 
        # typically desktops start with o/f/u/O/F/U
        case "$state" in
            f) # free (empty), inactive
                output+="%{A1:bspc desktop -f $name:} ${f_free} $name ${reset} %{A}"
                ;;
            F) # free (empty), FOCUSED -> GREY TEXT + YELLOW UNDERLINE
                output+="%{A1:bspc desktop -f $name:} ${f_focused_free} $name ${reset} %{A}"
                ;;
            o) # occupied, inactive
                output+="%{A1:bspc desktop -f $name:} ${f_occupied} $name ${reset} %{A}"
                ;;
            O) # occupied, FOCUSED -> WHITE TEXT + YELLOW UNDERLINE
                output+="%{A1:bspc desktop -f $name:} ${f_focused_occupied} $name ${reset} %{A}"
                ;;
            u|U) # urgent
                output+="%{A1:bspc desktop -f $name:} ${f_urgent} $name! ${reset} %{A}"
                ;;
        esac
    done
    echo "$output"
done