#RUN: %fish %s
#REQUIRES: command -v tmux

isolated-tmux-start

# Check no collapse
mkdir -p a/b
echo > a/b/f1
echo > a/b/f2
isolated-tmux send-keys 'HOME=$PWD ls ~/a/b/' Tab
t-sync
t-capture
# CHECK: prompt 0> HOME=$PWD ls ~/a/b/f
# CHECK: ~/a/b/f1  ~/a/b/f2

# Check collapse
isolated-tmux send-keys C-c
mkdir -p dddddd/eeeeee
echo > dddddd/eeeeee/file1
echo > dddddd/eeeeee/file2
isolated-tmux send-keys 'HOME=$PWD ls ~/dddddd/eeeeee/' Tab
t-sync
t-capture
# CHECK: prompt 0> HOME=$PWD ls ~/dddddd/eeeeee/file
# CHECK: …/file1  …/file2
