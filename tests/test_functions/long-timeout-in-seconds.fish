function long-timeout-in-seconds
    if set -q CI
        echo 60
    else
        echo 3
    end
end
