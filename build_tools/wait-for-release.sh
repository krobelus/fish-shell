#!/bin/sh

# Wait until the given release is public.

set -eux

repository_owner=$1
remote=$2
tag=$3
expected_commit=$4

while ! {
    repo_api_url=https://api.github.com/repos/$repository_owner/fish-shell
    curl -sL \
        -H "Accept: application/vnd.github+json" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        "$repo_api_url/releases/tags/$tag" |
        python -c "$(cat <<-EOF
			import json, sys
			release = json.load(sys.stdin)
			if release.get("draft", None) == False:
			    sys.exit(0)
			sys.exit(1)
			EOF
        )"
    }
do
    sleep 30s
done

actual_tag=$(git ls-remote $remote | awk '$2 == "refs/tags/'"$tag"'" { print $1 }')
[ "$expected_tag" = "$actual_tag" ]
