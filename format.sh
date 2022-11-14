#!/bin/bash
HS_FILES=$(git ls-files '*.hs')
ormolu --mode inplace $HS_FILES && echo "Ormolu success!"
