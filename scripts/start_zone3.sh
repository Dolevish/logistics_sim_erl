#!/bin/bash
# סקריפט הפעלה עבור מחשב מנהל אזורים 3

echo "Starting Zone Manager Node 3..."
cd /home/dolev/Desktop/logistics_sim

# קומפילציה של הפרויקט
rebar3 compile

# הפעלת הנוד
erl -name zone3@192.168.64.3 \
    -setcookie logistics_cookie \
    -config config/zone3_computer \
    -pa _build/default/lib/*/ebin \
    -eval "application:start(logistics_sim)." \
    -eval "io:format('Zone Manager Node 3 started successfully (managing zones 5,6)~n')."
