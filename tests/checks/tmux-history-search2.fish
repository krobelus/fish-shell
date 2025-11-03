#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start
isolated-tmux send-keys ': 1' Enter
isolated-tmux send-keys ': ' M-Up M-Down M-Up M-Up M-Up M-Down Enter
t-sync
isolated-tmux send-keys 'echo still alive' Enter
t-sync
t-capture
# CHECK: prompt 0> : 1
# CHECK: prompt 1> : 1
# CHECK: prompt 2> echo still alive
# CHECK: still alive
# CHECK: prompt 3>

isolated-tmux send-keys 'complete : -xa "foobar foobaz"' Enter
t-sync
isolated-tmux send-keys C-l ': fooba' Enter
t-sync
isolated-tmux send-keys C-p Tab
t-sync
t-capture
# CHECK: prompt 4> : fooba
# CHECK: prompt 5> : fooba
# CHECK: foobar  foobaz
