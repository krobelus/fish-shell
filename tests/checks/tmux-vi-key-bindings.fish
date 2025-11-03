#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start -C '
    set -g fish_key_bindings fish_vi_key_bindings
'

isolated-tmux send-keys 'echo 124' Escape
t-pause-after-legacy-escape
isolated-tmux send-keys v b y p i 3
t-sync
t-capture
# CHECK: [I] prompt 0> echo 1241234

isolated-tmux send-keys Escape
t-pause-after-legacy-escape
isolated-tmux send-keys e r 5
t-sync
t-capture
# CHECK: [N] prompt 0> echo 1241235
