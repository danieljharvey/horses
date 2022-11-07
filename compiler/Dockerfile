# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM haskell:9.4.3-buster as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build :mimsa-server --system-ghc --dependencies-only 

# -------------------------------------------------------------------------------------------
FROM haskell:9.4.3-buster as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build :mimsa-server --system-ghc 

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

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
