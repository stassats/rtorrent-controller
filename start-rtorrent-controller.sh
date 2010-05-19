#!/bin/sh

exec screen -d -m -S ccl-rtorrent ccl \
    --eval "(:asd :rtorrent-controller)" \
    --eval "(rtorrent-controller:inotify-loop)"
