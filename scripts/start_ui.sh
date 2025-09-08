#!/bin/bash
# סקריפט הפעלה עבור מחשב ממשק המשתמש

echo "Starting UI Node..."
cd /home/dolev/Desktop/logistics_sim

# קומפילציה של הפרויקט
rebar3 compile

# הפעלת הנוד
erl -name ui@192.168.64.3 \
    -setcookie logistics_cookie \
    -config config/ui_computer \
    -pa _build/default/lib/*/ebin \
    -eval "application:start(logistics_sim)." \
    -eval "io:format('UI Node started successfully~nWeb server available at http://localhost:8080~n')."
