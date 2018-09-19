# Builds binaries for both the server and unigen, and then copies
# both binaries to a lightweight final image.
#
# Build by running 'docker build -t sweetpea/server .' from this directory.
# Push with 'docker push sweetpea/server:latest'
# Make sure you've logged in with 'docker login'
#
# Run the image locally with:
#   docker run -d -p 8080:8080 sweetpea/server:latest
#
# Then hit the API on localhost:8080

# UNIGEN
FROM debian:jessie-slim as unigen-builder

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    apt-get install --yes build-essential \
    git \
    autoconf \
    libtool \
    zlib1g-dev

RUN git clone https://bitbucket.org/kuldeepmeel/unigen && \
    cd unigen/ugen2 && \
    make -f Makefile.cvs && \
    mkdir build && \
    cd build && \
    ../configure --enable-static-link && \
    make

# SERVER
FROM haskell:8.2.2 as server-builder

RUN git clone https://github.com/anniecherk/sweetpea-core && \
    cd sweetpea-core && \
    stack install

# FINAL IMAGE
# https://futtetennismo.me/posts/docker/2017-11-24-docker-haskell-executables.html
FROM fpco/haskell-scratch:integer-gmp

COPY --from=unigen-builder /unigen/ugen2/build/unigen /bin/
COPY --from=server-builder /root/.local/bin/server /bin/

EXPOSE 8080

ENTRYPOINT ["server"]
