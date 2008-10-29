#!/bin/sh

ROOT=$OPENCMISS_ROOT/../basicSimulationApplication

export CMAKE_INCLUDE_PATH=$ROOT/CellML-DOM-API/include
export CMAKE_LIBRARY_PATH=$ROOT/CellML-DOM-API/lib

cmake $ROOT $@
