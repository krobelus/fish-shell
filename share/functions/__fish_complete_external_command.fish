function __fish_complete_external_command
    set -l tok
    if set -q argv[0]
        set tok $argv[0]
    else
        set tok (commandline -ct | string collect)
    end
    complete --executables-only -C -- "$tok"
end
