#!/usr/bin/env bash

CURRENT=$(dirname $0)

started() {  
    port=$(lsof -n -i $1:$2 | sed "1d" | awk '{ print $2 }')
    if [ $port ] ; then 
        return 1
    fi
    return 0
}

debug() {
    echo "DEBUG: $1"
}

started "udp" 9801
gateway_started=$?

started "tcp" 2939
server_api_started=$?

if [ $server_api_started == 1 -a $gateway_started == 1 ]; then 
    debug "sending multiple messages"

    $CURRENT/mqtt-sn-pub -t test_topic -m test_message_value+1 -h 127.0.0.1 -p 9801 -q 1
    $CURRENT/mqtt-sn-pub -t test_topic -m test_message_value+2 -h 127.0.0.1 -p 9801 -q 1
    $CURRENT/mqtt-sn-pub -t test_topic -m test_message_value+3 -h 127.0.0.1 -p 9801 -q 1
    $CURRENT/mqtt-sn-pub -t test_topic -m test_message_value+4 -h 127.0.0.1 -p 9801 -q 1  
else 
    debug "server api or the gateway were not started, try to start the app in other terminal using ./start-app.sh"
fi

