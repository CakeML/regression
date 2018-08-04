#!/bin/sh
## A (possibly outdated) tool to help with saving a worker's build
## state for inspection elsewhere.
set -e
NAME=cakeml-$1
HOLDIR=$(cat ${NAME}/HOLDIR)
tar --create --file ${NAME}.tar --exclude-from="${HOLDIR}/tools-poly/rebuild-excludes.txt" --exclude="${HOLDIR}/.git" --exclude="${HOLDIR}/help/Docfiles" --exclude="${HOLDIR}/help/theorygraph" --transform="s|^${HOLDIR}|${NAME}/HOL|r" ${HOLDIR}
tar --append --file ${NAME}.tar --exclude-from="${NAME}/developers/rebuild-excludes" --exclude="${NAME}/.git" --transform="s|^${NAME}|${NAME}/cakeml|r" ${NAME}
gzip ${NAME}.tar
rsync -Pvz ${NAME}.tar.gz xrchz@xrchz.strongspace.com:/strongspace/xrchz/public/

# Developer Recipe
#
# 1.  Download http://strongspace.com/xrchz/public/cakeml-XXX.tar.gz
# 2.  tar -xzf cakeml-XXX.tar.gz && cd cakeml-XXX
# 3.  rebuild
#     -   HOL:
#         1. poly --script tools/smart-configure.sml
#         2. bin/build --relocbuild --nograph
#            This will eventually fail complaining about
#            missing help/Docfiles/HTML - that is OK
#     -   CakeML:
#         1. Manually build some heaps (until https://github.com/CakeML/cakeml/issues/389 is solved):
#            cd cakeml/semantics/proofs
#            /path/to/the/above/HOL/bin/Holmake --relocbuild heap
#            cd ../..
#         2. cd $(cat resume)
#         3. /path/to/the/above/HOL/bin/Holmake --relocbuild
#         4. After hacking, you could copy your .git into the cakeml directory
#            (from a working directory checked out to the the correct branch)
#            to make commits.
