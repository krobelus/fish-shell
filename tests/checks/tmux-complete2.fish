#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start

isolated-tmux send-keys 'touch ~/"path with spaces"' Enter
t-sync

isolated-tmux send-keys C-l 'cat ~/space' Tab
t-sync
t-capture
# CHECK: prompt 1> cat ~/path\ with\ spaces

# Clear screen.
isolated-tmux send-keys C-c

isolated-tmux send-keys '
    set -g fish_autosuggestion_enabled 0
    set -l FISH_TEST_VAR_1 /
    set -l FISH_TEST_VAR_2 /
' Enter C-l
t-sync
t-capture
# Note we keep prompt 1 because the above "set" commands don't bump $status_generation.
# CHECK: prompt 1>

isolated-tmux send-keys 'echo $FISH_TEST_v' Tab
t-sync
t-capture
# CHECK: prompt 1> echo $FISH_TEST_VAR_
# CHECK: $FISH_TEST_VAR_1  (Variable: /)  $FISH_TEST_VAR_2  (Variable: /)
