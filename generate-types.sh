#!/bin/bash

pushd compiler
stack exec swagger > swagger.json
popd

pushd ui
yarn
yarn typegen
popd
