#!/bin/bash

stack exec chotp-exe -- agent --host 127.0.0.1 --port 10001 &
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10002 &
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10003 &
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10004 &
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10005 &

sleep 1

stack exec chotp-exe -- master --host 127.0.0.1 --port 10006 --send-for 3 --wait-for 3 --with-seed 0