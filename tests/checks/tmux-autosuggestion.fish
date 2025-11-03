#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start
isolated-tmux send-keys 'echo "foo bar baz"' Enter C-l
isolated-tmux send-keys 'echo '
t-sync
isolated-tmux send-keys M-Right
t-capture
# CHECK: prompt 1> echo "foo bar baz"
t-sync

touch COMPL

# Regression test.
isolated-tmux send-keys C-u C-l ': sometoken' M-b c
t-sync
t-capture
# CHECK: prompt 1> : csometoken

# Test that we get completion autosuggestions also when the cursor is not at EOL.
isolated-tmux send-keys C-u 'complete nofilecomp -f' Enter C-l 'nofilecomp ./CO' C-a M-d :
t-sync
t-capture
# CHECK: prompt 2> : ./COMPL

isolated-tmux send-keys C-u C-k C-l ': ./CO'
t-sync
isolated-tmux send-keys A C-h
t-sleep-until 't-capture | grep COMPL' # autosuggestion computation is async.
t-capture
# CHECK: prompt 2> : ./COMPL

isolated-tmux send-keys C-u 'ech {' Left Left
t-sync
isolated-tmux send-keys o C-e C-h 'still alive' Enter
t-sync
t-capture
# CHECK: prompt {{\d+}}> echo still alive
# CHECK: still alive
# CHECK: prompt {{\d+}}>

isolated-tmux send-keys C-u 'echo (echo)' Enter
isolated-tmux send-keys C-l 'echo ('
t-sync
t-capture
# CHECK: prompt {{\d+}}> echo (echo)
