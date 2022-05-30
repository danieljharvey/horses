# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM haskell:9.2.2-buster as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

RUN apt-get update && apt-get install -y llvm

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build :mimsa-server --system-ghc --dependencies-only --ghc-options="-fllvm"

# -------------------------------------------------------------------------------------------
FROM haskell:9.2.2-buster as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN apt-get update && apt-get install -y llvm

RUN stack build :mimsa-server --system-ghc --ghc-options="-fllvm"

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
