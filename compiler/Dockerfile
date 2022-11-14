# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM haskell:9.4.3-buster as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# Docker build should not use cached layer if any of these is modified
COPY mimsa.cabal cabal.project cabal.project.freeze /opt/build/
RUN cabal update && cabal build mimsa:exe:mimsa-server --dependencies-only

# -------------------------------------------------------------------------------------------
FROM haskell:9.4.3-buster as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.cabal /root/.cabal
COPY . /opt/build/

WORKDIR /opt/build

RUN cabal update && cabal build mimsa:exe:mimsa-server

RUN mv "$(cabal list-bin mimsa:exe:mimsa-server)" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:22.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app
 
RUN apt-get update && apt-get install -y libncurses5 libnuma-dev 

COPY --from=build /opt/build/bin .
EXPOSE 8999
CMD ["/opt/app/mimsa-server"]
