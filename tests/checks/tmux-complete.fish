#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux

# Don't escape existing token (#7526).
echo >file-1
echo >file-2
isolated-tmux send-keys 'HOME=$PWD ls ~/' Tab
$sleep
isolated-tmux capture-pane -p
# Note the contents may or may not have the autosuggestion appended - it is a race.
# CHECK: prompt 0> HOME=$PWD ls ~/file-{{1?}}
# CHECK: ~/file-1  ~/file-2

# No pager on single smartcase completion (#7738).
isolated-tmux send-keys C-u C-l 'mkdir cmake CMakeFiles' Enter C-l \
    'cat cmake' Tab
$sleep
isolated-tmux capture-pane -p
# CHECK: prompt 1> cat cmake/

# Correct case in pager when prefixes differ in case (#7743).
isolated-tmux send-keys C-u C-l 'complete -c foo2 -a "aabc aaBd" -f' Enter C-l \
    'foo2 A' Tab
$sleep
isolated-tmux capture-pane -p
# The "bc" part is the autosuggestion - we could use "capture-pane -e" to check colors.
# CHECK: prompt 2> foo2 aabc
# CHECK: aabc  aaBd
