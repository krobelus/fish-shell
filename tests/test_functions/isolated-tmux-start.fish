function isolated-tmux-start --wraps fish
    set -l tmpdir (mktemp -d)
    cd $tmpdir

    echo 'set -g mode-keys emacs' >.tmux.conf

    function isolated-tmux --inherit-variable tmpdir --wraps tmux
        # tmux can't handle session sockets in paths that are too long, and macOS has a very long
        # $TMPDIR, so use a relative path - except macOS doesn't have `realpath --relative-to`...
        # Luckily, we don't need to call tmux from other directories, so just make sure no one
        # does by accident.
        if test $PWD != $tmpdir
            echo "error: isolated-tmux must always be run from the same directory." >&2
            return 1
        end
        tmux -S .tmux-socket -f .tmux.conf $argv
    end

    function isolated-tmux-cleanup --on-event fish_exit --inherit-variable tmpdir
        isolated-tmux kill-server
        rm -r $tmpdir
    end

    mkfifo .fifo # for open-fifo

    set -l fish (status fish-path)
    set -l size -x 80 -y 10
    isolated-tmux new-session $size -d $fish -C '
        function semaphore-post
            open-fifo ">"
            or exit
        end
         # HACK Vi mode might not be loaded yet.
        for mode in (bind --list-modes) insert replace replace_one visal
            bind -M $mode \e\]fish-semaphore-post\a semaphore-post
        end
        # This is similar to "tests/interactive.config".
        function fish_greeting; end
        function fish_prompt; printf "prompt $status_generation> "; end
        # No autosuggestion from older history.
        set fish_history ""
        # No transient prompt.
        set fish_transient_prompt 0
    ' $argv
    # Set the correct permissions for the newly created socket to allow future connections.
    # This is required at least under WSL or else each invocation will return a permissions error.
    chmod 777 .tmux-socket

    # Resize window so we can attach to tmux session without changing panel size.
    isolated-tmux resize-window $size

    # Loop a bit, until we get an initial prompt.
    t-sleep-until 'test -n "$(t-capture)" # initial prompt'
end

function t-sync
    isolated-tmux send-keys \e\]fish-semaphore-post\a
    open-fifo "<" # semaphore wait
    or exit
end

function t-send --wraps 'tmux send-keys'
    isolated-tmux send-keys $argv
end

function t-capture --wraps 'tmux capture-pane -p'
    isolated-tmux capture-pane -p $argv |
        # N.B. littlecheck allows empty lines.
        # Let's hope this doesn't cause too much chaos.
        string replace -a '^[]fish-semaphore-post^G⏎' '' |
        string replace -a '^[]fish-semaphore-post^G' ''
end

function t-pause-after-legacy-escape
    # Disambiguate legacy escape from alt
    sleep .1
end

function t-sleep-until
    set -l cmd $argv[1]
    set -l i 0
    set -l max_attempts 20
    set -l interval (math (long-timeout-in-seconds) / $max_attempts)
    while [ $i -lt $max_attempts ] && not eval "$cmd" >/dev/null
        sleep $interval
        set i (math $i + 1)
    end
    if [ $i -eq $max_attempts ]
        printf '%s\n' "timeout waiting for $cmd" >&2
        exit 1
    end
end
