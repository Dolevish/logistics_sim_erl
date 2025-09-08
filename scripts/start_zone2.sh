#!/bin/bash
# סקריפט הפעלה עבור מחשב מנהל אזורים 2

echo "Starting Zone Manager Node 2..."
cd /home/dolev/Desktop/logistics_sim

# קומפילציה של הפרויקט
rebar3 compile

# הפעלת הנוד
erl -name zone2@192.168.64.3 \
    -setcookie logistics_cookie \
    -config config/zone2_computer \
    -pa _build/default/lib/*/ebin \
    -eval "application:start(logistics_sim)." \
    -eval "io:format('Zone Manager Node 2 started successfully (managing zones 3,4)~n')."
