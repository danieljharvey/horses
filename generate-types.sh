#!/bin/bash

pushd compiler
make install
stack exec mimsa-server generate-swagger > swagger.json
popd

pushd ui
yarn
yarn typegen
yarn format
popd
