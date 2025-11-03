#RUN: %fish %s
#REQUIRES: command -v tmux
# Somehow $LINES is borked on NetBSD?
#REQUIRES: test $(uname) != NetBSD

isolated-tmux-start -C '
    bind ctrl-q "functions --erase fish_right_prompt" "commandline \'\'" clear-screen
    set -g fish_autosuggestion_enabled 0
    bind ctrl-g "__fish_echo commandline --current-job"
    bind ctrl-t \'__fish_echo echo cursor is at offset $(commandline --cursor --current-token) in token\'
'

isolated-tmux send-keys 'echo LINES $LINES' Enter
t-sync
t-capture
# CHECK: prompt 0> echo LINES $LINES
# CHECK: LINES 10
# CHECK: prompt 1>

isolated-tmux send-keys 'bind alt-g "commandline -p -C -- -4"' Enter C-l
isolated-tmux send-keys 'echo bar|cat' \eg foo
t-sync
t-capture
# CHECK: prompt 2> echo foobar|cat

isolated-tmux send-keys C-k C-u C-l 'commandline -i "\'$(seq $LINES)" scroll_here' Enter
t-sync
t-capture
# CHECK: 2
# CHECK: 3
# CHECK: 4
# CHECK: 5
# CHECK: 6
# CHECK: 7
# CHECK: 8
# CHECK: 9
# CHECK: 10
# CHECK: scroll_here

isolated-tmux send-keys C-c
t-sync
isolated-tmux send-keys C-l
t-sync
isolated-tmux send-keys '
    function fish_right_prompt
        echo right-prompt
    end
' 'commandline -i ": \'$(seq (math $LINES \* 2))\'"' Enter Enter
t-sync
# Detect whether this terminal adds a blank line after prompts with right-prompt.
# Check if the first line captured with -S -12 is blank.
set -l probe (t-capture -S -12 | head -1)
# If it's blank or just whitespace, we need -13 to go back one more to get the prompt line
string match -qr '^\s*$' -- $probe && set -l offset -13 || set -l offset -12
t-capture -S $offset
# CHECK: prompt 4> commandline -i ": '$(seq (math $LINES \* 2))'"            right-prompt
# CHECK: prompt 5> : '1                                                      right-prompt
# CHECK: 2
# CHECK: 3
# CHECK: 4
# CHECK: 5
# CHECK: 6
# CHECK: 7
# CHECK: 8
# CHECK: 9
# CHECK: 10
# CHECK: 11
# CHECK: 12
# CHECK: 13
# CHECK: 14
# CHECK: 15
# CHECK: 16
# CHECK: 17
# CHECK: 18
# CHECK: 19
# CHECK: 20'
# CHECK: prompt 6>                                                           right-prompt

# Soft-wrapped commandline with omitted right prompt.
isolated-tmux send-keys 'commandline -i "echo $(printf %0"$COLUMNS"d)"' Enter C-l Enter
t-sync
t-capture
# CHECK: prompt {{\d+}}> echo 00000000000000000000000000000000000000000000000000000000000000000
# CHECK: 000000000000000
# CHECK: 00000000000000000000000000000000000000000000000000000000000000000000000000000000
# CHECK: prompt {{\d+}}>                                                           right-prompt

isolated-tmux send-keys C-q 'echo | echo\;' M-Enter 'another job' C-b C-b C-g
t-sync
t-capture
# CHECK: prompt {{\d+}}> echo | echo;
# CHECK:          another job
# CHECK: another job
# CHECK: prompt {{\d+}}> echo | echo;
# CHECK:          another job

isolated-tmux send-keys C-q 'echo foobar' Left Left Left C-t
t-sync
# CHECK: prompt {{\d+}}> echo foobar
# CHECK: cursor is at offset 3 in token
# CHECK: prompt {{\d+}}> echo foobar
t-capture

isolated-tmux send-keys C-a C-k \
    'bind ctrl-x,a "__fish_echo echo line=(commandline --line) column=(commandline --column)"' \
    Enter \
    C-l "echo '1" Enter 2
t-sync
isolated-tmux send-keys C-x a C-a Up C-x a
t-sync
# CHECK: prompt {{\d+}}> echo '1
# CHECK: 2
# CHECK: line=2 column=2
# CHECK: prompt {{\d+}}> echo '1
# CHECK: 2
# CHECK: line=1 column=1
# CHECK: prompt {{\d+}}> echo '1
# CHECK: 2
t-capture
