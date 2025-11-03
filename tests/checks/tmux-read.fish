#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start -C '
    function fish_greeting
        set -l name (read)
        echo hello $name
    end
'

isolated-tmux send-keys name Enter 'echo foo' Enter
t-sync
t-capture
# CHECK: read> name
# CHECK: hello name
# CHECK: prompt 0> echo foo
# CHECK: foo
# CHECK: prompt 1>
