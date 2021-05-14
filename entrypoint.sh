#!/bin/bash -l
set -e
echo "Looking for new app installations...."
if [ "$username" ]; then
Rscript -e "setupuniverse::create_universe_repo('$username')"
else
Rscript -e "setupuniverse::setup_universes()"
fi
echo "Action complete!"
