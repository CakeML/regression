all : server worker

server : src/server.sml lib/serverLib.sml lib/apiLib.sml lib/utilLib.sml github-token cakeml-token
	polyc src/server.sml -o server

worker : src/worker.sml lib/apiLib.sml lib/utilLib.sml cakeml-token name
	polyc src/worker.sml -o worker

cakeml-token :
	sha1sum src/worker.sml > cakeml-token

name :
	uname -norm > name

clean :
	rm -fr server worker name cakeml-token

.PHONY : clean all
