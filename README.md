# Action: setup-universes

This action creates and deletes [r-universe monorepos](https://github.com/r-universe) based changes on CRAN and installations of the github app. This action runs on schedule once per day from the [control-room](https://github.com/r-universe-org/control-room) and also immediately via webhook dispatch upon any (un)installations of the [r-universe github app](https://github.com/apps/r-universe).
