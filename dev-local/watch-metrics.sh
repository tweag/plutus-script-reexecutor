#!/usr/bin/env bash

while true; do
    clear
    curl -s http://localhost:8090/metrics |
        grep -v -E '^(#)' |
        awk '{ printf "\033[1;36m%-60s\033[0m \033[1;33m%s\033[0m\n", $1, $2 }'
    sleep 1
done
