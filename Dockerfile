FROM fedora:latest

ARG POLY_GIT=https://github.com/polyml/polyml.git
ARG POLY_DIR=/polyml
ARG OVEN_GIT=https://github.com/CakeML/regression.git
ARG OVEN_DIR=/oven

# basic stuff
RUN dnf -y install gcc-c++ git curl make time psmisc diffutils && \
    useradd -u 1050 -m oven && \
    mkdir ${OVEN_DIR} && \
    chown oven:oven ${OVEN_DIR}

# polyml
RUN git clone ${POLY_GIT} ${POLY_DIR} && \
    cd ${POLY_DIR} && \
    git checkout v5.8.1 && \
    ./configure && \
    make && \
    make install && \
    cd / && \
    rm -fr ${POLY_DIR}

# drop privileges
USER oven

# cakeml-regression
RUN git clone ${OVEN_GIT} ${OVEN_DIR} && \
    cd ${OVEN_DIR} && \
    sha1sum worker.sml > cakeml-token && \
    polyc worker.sml -o worker && \
    git config --global user.email oven && \
    git config --global user.name oven

WORKDIR ${OVEN_DIR}
ENTRYPOINT ["/bin/sh", "-c", "uname -norm > /oven/name && exec /oven/worker"]
