#!/bin/bash
# סקריפט הפעלה עבור מחשב מרכז הבקרה

echo "Starting Control Center Node..."
cd /home/dolev/Desktop/logistics_sim

# קומפילציה של הפרויקט
rebar3 compile

# הפעלת הנוד
erl -name control@192.168.64.3 \
    -setcookie logistics_cookie \
    -config config/control_computer \
    -pa _build/default/lib/*/ebin \
    -eval "application:start(logistics_sim)." \
    -eval "io:format('Control Center Node started successfully~n')."
