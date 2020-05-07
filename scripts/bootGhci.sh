#!/bin/bash

set -e

normal()  { tput sgr0;    }
red()     { tput setaf 1; }
green()   { tput setaf 2; }
yellow()  { tput setaf 3; }
blue()    { tput setaf 4; }
magenta() { tput setaf 5; }
cyan()    { tput setaf 6; }
white()   { tput setaf 7; }

NODES=$(docker ps --filter "publish=10000" --format "{{.Ports}}" | cut -d' ' -f5 | cut -d':' -f2 | cut -d'-' -f1);

arr=($NODES)

NODE=${arr[0]}

if [[ ! $NODE ]]; then
  echo "$(red)No nodes found, are you sure the docker testnet is running?$(normal)"
  exit
else
  echo "Booting with node at port: $(green)$NODE$(normal)"
fi


NODE_URL="localhost:$NODE" \
ES_URL="http://localhost:9200" \
SIMPLEID_URL="http://localhost:8000" \
stack ghci
