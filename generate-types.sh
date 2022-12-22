#!/bin/bash

make generate-swagger > swagger.json

pushd ui || exit
yarn
yarn typegen
yarn format
popd || return
