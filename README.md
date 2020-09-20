Automated regression test infrastructure for CakeML.

[apiLib.sml](apiLib.sml):
The API that the server and worker agree on.

[flock.sml](flock.sml):
Maintenance tool: acquire the lock that is used by the server to run a
command without interfering with the server.

[server.sml](server.sml):
Implements the server-side regression-test API as a CGI program.

[serverLib.sml](serverLib.sml):
Functions for manipulating the queues in the filesystem on the server,
including for getting information from GitHub with which to update them.

[utilLib.sml](utilLib.sml):
Small library of useful code.

[worker.sml](worker.sml):
Worker that claims and runs regression test jobs.

[Dockerfile](Dockerfile):
Worker docker container for easy deployment:

    docker build -t oven .
    # Unlimited RAM (only for dedicated machines w/ ample swap)
    docker run -d --hostname=my-builder-name --name=container-name oven

    # OR: Limit RAM to 70,000 MB
    docker run -d --hostname=my-builder-name --name=container-name \
        -e POLY_CLINE_OPTIONS="--maxheap 70000" oven

    # Maintenance commands
    docker exec container-name /oven/worker --help

    # Skip 20 minute timeout after a refresh
    docker exec container-name killall curl
