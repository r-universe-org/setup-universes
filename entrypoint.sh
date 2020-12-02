#!/bin/bash -l
set -e
echo "Looking for new app installations...."
Rscript -e "setupuniverse::setup_universes()"
echo "Action complete!"
