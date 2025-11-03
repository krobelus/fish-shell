function open-fifo -a redirection
    timeout (long-timeout-in-seconds) sh -c "exec $redirection .fifo"
    set -l saved_status $status
    if test $saved_status -eq 124
        echo >&2 "error: timed out in 'exec $redirection .fifo'"
    end
    return $saved_status
end
