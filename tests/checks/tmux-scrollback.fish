#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start
isolated-tmux send-keys 'bind ctrl-g "commandline -f scrollback-push scrollback-push clear-screen"' Enter C-g
t-sync
t-capture
# CHECK: prompt 1>
