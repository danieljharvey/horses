#!/bin/bash

pushd compiler
stack install
stack exec mimsa-server generate-swagger > swagger.json
popd

pushd ui
yarn
yarn typegen
popd
