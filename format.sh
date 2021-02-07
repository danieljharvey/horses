#!/bin/bash

ormolu --mode inplace $(find . -name '*.hs') && echo "Ormolu success!"
