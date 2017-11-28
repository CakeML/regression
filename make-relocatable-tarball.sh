#!/bin/sh
set -e
NAME=cakeml-$1
HOLDIR=$(cat ${NAME}/HOLDIR)
tar --create --file ${NAME}.tar --exclude-from="${HOLDIR}/tools-poly/rebuild-excludes.txt" --exclude="${HOLDIR}/help/Docfiles" --exclude="${HOLDIR}/help/theorygraph" --transform="s|^${HOLDIR}|${NAME}/HOL|r" ${HOLDIR}
tar --append --file ${NAME}.tar --exclude-from="${NAME}/developers/rebuild-excludes" --transform="s|^${NAME}|${NAME}/cakeml|r" ${NAME}
gzip ${NAME}.tar
rsync -Pvz ${NAME}.tar.gz xrchz@xrchz.strongspace.com:/strongspace/xrchz/public/

# Developer Recipe
#
# 1.  download stuff
# 2.  untar working directories in fresh locations
# 3.  rebuild
#     -   HOL will require
#         1. poly --script tools/smart-configure.sml
#         2. bin/build --relocbuild --nograph
#     -   CakeML
#         1. cd cakeml/`cat cakeml/resume`
#         2. fix Lem stuff: remove addancs dependencies in semantics/Holmakefile
#         3. /path/to/the/above/HOL/bin/Holmake --relocbuild
