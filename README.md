Automated regression test infrastructure for CakeML.

[Dockerfile](Dockerfile):
Worker docker container for easy deployment `docker run -d agomezl/oven`.

[apiLib.sml](apiLib.sml):
The API that the server and worker agree on.

[flock.sml](flock.sml):
Outdated maintenance tool: acquire the lock that used to be used by the
server, hoping to run a command without interfering with the server.

[make-relocatable-tarball.sh](make-relocatable-tarball.sh):
A (possibly outdated) tool to help with saving a worker's build
state for inspection elsewhere.

[server.sml](server.sml):
Implements the server-side regression-test API as a CGI program.

[serverLib.sml](serverLib.sml):
Functions for manipulating the queues in the filesystem on the server,
including for getting information from GitHub with which to update them.

[style.css](style.css):
Style file for server-generated web pages.

[utilLib.sml](utilLib.sml):
Small library of useful code.

[worker.sml](worker.sml):
Worker that claims and runs regression test jobs.
