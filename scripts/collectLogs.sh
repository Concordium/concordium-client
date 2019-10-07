#!/bin/bash
set -e

echo "Removing docker-compose-logs.zip"
rm docker-compose-logs.zip | true

echo "Exporting logs for all bakers..."

NODES=$(docker ps --filter "publish=10000" --format "{{.Names}}");
for i in $NODES; do
  echo "--> $i.log"
  docker logs -t $i >& $i.log
done

echo "Creating docker-compose-logs.zip"
zip docker-compose-logs.zip p2p-client_baker_*

echo "Cleaning up log files"
rm p2p-client_baker_*.log
