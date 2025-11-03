#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start -C '
    function fish_prompt
        if set -q transient
            printf "> "
            set --erase transient
        else
            printf "> full prompt > "
        end
    end
    bind ctrl-j "set transient true" repaint execute
'

isolated-tmux send-keys 'echo foo' C-j
t-sync
t-capture
# CHECK: > echo foo
# CHECK: foo
# CHECK: > full prompt >

# Regression test for transient prompt with single-line prompts.
isolated-tmux send-keys C-u '
    set -g fish_transient_prompt 1
    function fish_prompt
        printf "\$ "
    end
'
isolated-tmux send-keys C-l Enter Enter
t-sync
t-capture
# CHECK: $
# CHECK: $
# CHECK: $

# Test that multi-line transient are properly cleared.
isolated-tmux send-keys C-u C-l '
    function fish_prompt
        if contains -- --final-rendering $argv
            printf "final line%d\n" 1 2
        else
            printf "transient line%d\n" 1 2
        end
    end
'
isolated-tmux send-keys C-l Enter
t-sync
t-capture
# CHECK: final line1
# CHECK: final line2
# CHECK: transient line1
# CHECK: transient line2

# Test that multi-line initial prompt is properly cleared with single-line
# final.
isolated-tmux send-keys C-u C-l '
    function fish_prompt
        if contains -- --final-rendering $argv
            echo "2> "
        else
            echo "transient prompt line"
            echo "1> "
        end
    end
'
isolated-tmux send-keys C-l 'echo foo' Enter
t-sync
t-capture
# CHECK: 2> echo foo
# CHECK: foo
# CHECK: transient prompt line
# CHECK: 1>

# Test that multi-line initial prompt is properly cleared with single-line
# final.
isolated-tmux send-keys C-u C-l
isolated-tmux send-keys 'echo foo \\' Enter
isolated-tmux send-keys bar Enter
t-sync
t-capture
# CHECK: 2> echo foo \
# CHECK:        bar
# CHECK: foo bar
# CHECK: transient prompt line
# CHECK: 1>
