# win

> Manage bspwm windows from the command line: list, hide, unhide, move, focus, close.
> Windows are addressed by their index in the bare listing.
> Hidden windows still mark their workspace as occupied; `unhide` recovers them.

- List windows (index, workspace, class, title; `○` marks hidden):

`win`

- Hide the focused window:

`win hide`

- Hide a window by index:

`win hide {{3}}`

- Unhide the only hidden window (lists candidates when there are several):

`win unhide`

- Unhide a window by index, or all hidden windows:

`win unhide {{3}}`

`win unhide all`

- Move the focused window to a workspace:

`win move {{2}}`

- Move a window by index to a workspace:

`win move {{3}} {{2}}`

- Jump to a window (unhides it if hidden):

`win focus {{3}}`

- Close a window (focused if no index):

`win close {{3}}`
