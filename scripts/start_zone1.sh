#!/bin/bash
# סקריפט הפעלה עבור מחשב מנהל אזורים 1

echo "Starting Zone Manager Node 1..."
cd /home/dolev/Desktop/logistics_sim

# קומפילציה של הפרויקט
rebar3 compile

# הפעלת הנוד
erl -name zone1@192.168.64.3 \
    -setcookie logistics_cookie \
    -config config/zone1_computer \
    -pa _build/default/lib/*/ebin \
    -eval "application:start(logistics_sim)." \
    -eval "io:format('Zone Manager Node 1 started successfully (managing zones 1,2)~n')."
