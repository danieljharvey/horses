#!/bin/bash

pushd compiler
stack exec swagger > swagger.json
popd

[[ -d ./typegen ]] && rm -R typegen

openapi-generator generate -i ./compiler/swagger.json -g typescript-axios -c ./swagger.config.json -o ./typegen

[[ -d ./mimsa-ui/src/generated ]] && rm -R ./ui/src/generated/

mkdir ./ui/src/generated

cp -a typegen/mimsa-types/. ui/src/generated/

[[ -d ./typegen ]] && rm -R typegen

