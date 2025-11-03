#RUN: %fish %s
#REQUIRES: command -v tmux

# The default history-delete binding is shift-delete which
# won't work on terminals that don't support CSI u, so rebind.
isolated-tmux-start -C '
    set -g fish_autosuggestion_enabled 0
    bind alt-d history-delete or backward-delete-char
'

isolated-tmux send-keys 'true needle' Enter
# CHECK: prompt 0> true needle
t-sync
isolated-tmux send-keys 'true hay ee hay' Enter
# CHECK: prompt 1> true hay ee hay
t-sync
isolated-tmux send-keys C-p C-a M-f M-f M-f M-.
# CHECK: prompt 2> true hay needle hay
t-sync
t-capture

isolated-tmux send-keys C-e C-u true Up Up Escape
t-pause-after-legacy-escape
t-capture | grep 'prompt 2'
# CHECK: prompt 2> true
isolated-tmux send-keys C-z _
t-sync
t-capture | grep 'prompt 2'
# CHECK: prompt 2> _

# When history pager fails to find a result, copy the search field to the command line.
isolated-tmux send-keys C-e C-u C-r "echo no such command in history"
t-sync
t-send Enter
# CHECK: prompt 2> echo no such command in history
t-capture | grep 'prompt 2'
isolated-tmux send-keys C-c

isolated-tmux send-keys C-r hay/shmay
isolated-tmux send-keys C-w C-h
isolated-tmux send-keys Enter
# CHECK: prompt 2> true hay ee hay
t-capture | grep 'prompt 2>'
isolated-tmux send-keys C-c

isolated-tmux send-keys 'echo 1' Enter 'echo 2' Enter 'echo 3' Enter
isolated-tmux send-keys C-l echo Up M-d
t-sync
t-capture
#CHECK: prompt 5> echo 2
isolated-tmux send-keys C-c

isolated-tmux send-keys "echo sdifjsdoifjsdoifj" Enter
t-sync
t-capture | grep "^sdifjsdoifjsdoifj\|prompt 6>"
# CHECK: sdifjsdoifjsdoifj
# CHECK: prompt 6>
isolated-tmux send-keys C-e C-u C-r
t-sync
isolated-tmux send-keys "echo sdifjsdoifjsdoifj"
t-sync
isolated-tmux send-keys M-d # alt-d
t-sync
t-capture | grep "(no matches)"
# CHECK: (no matches)
isolated-tmux send-keys Enter C-e C-u "echo foo" Enter
t-sync
t-capture | grep "^foo\|prompt 7>"
# CHECK: foo
# CHECK: prompt 7>
