#!/bin/sh
set -e
pushd $1
NAME=cakeml-job-$1
tar --create --file ${NAME}.tar --exclude-from=HOL/tools-poly/rebuild-excludes.txt --exclude=HOL/help/Docfiles --exclude=HOL/help/theorygraph --transform="s|^|${NAME}/|" HOL
tar --append --file ${NAME}.tar --exclude-from=cakeml/developers/rebuild-excludes --transform="s|^|${NAME}/|" cakeml
gzip ${NAME}.tar
rsync -Pvz ${NAME}.tar.gz xrchz@xrchz.strongspace.com:/strongspace/xrchz/public/
popd

# Developer Recipe
#
# 1.  download stuff
# 2.  untar working directories in fresh locations
# 3.  rebuild
#     -   HOL will require
#         1. poly < tools/smart-configure.sml
#         2. bin/build --relocbuild
#     -   CakeML
#         1. cd cakeml/`cat cakeml/resume`
#         2. Holmake --qof -k --relocbuild
