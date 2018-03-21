FROM fedora:27

ARG POLY_GIT=https://github.com/polyml/polyml.git
ARG POLY_DIR=/polyml
ARG OVEN_GIT=https://github.com/CakeML/regression.git
ARG OVEN_DIR=/oven

# basic stuff
RUN dnf -y install gcc-c++ git curl make time

# polyml
RUN git clone ${POLY_GIT} ${POLY_DIR} && \
    cd ${POLY_DIR} && \
    ./configure && \
    make && \
    make install && \
    cd / && \
    rm -fr ${POLY_DIR}

# cakeml-regression
RUN git clone ${OVEN_GIT} ${OVEN_DIR} && \
    cd ${OVEN_DIR} && \
    sha1sum worker.sml > cakeml-token && \
    uname -norm > name && \
    polyc worker.sml -o worker


WORKDIR ${OVEN_DIR}
ENTRYPOINT ["/oven/worker"]
