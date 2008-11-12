cmake_minimum_required(VERSION 2.6)

PROJECT(bsa)

ENABLE_LANGUAGE(Fortran)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${bsa_SOURCE_DIR})

FIND_PACKAGE(CellML REQUIRED)
FIND_PACKAGE(CCGS REQUIRED)
FIND_PACKAGE(Redland REQUIRED)
FIND_PACKAGE(LibXml2 REQUIRED QUIET)

SET (CMAKE_CXX_FLAGS "-Wall -Werror ${LIBXML2_DEFINITIONS} ${Redland_DEFINITIONS}")

INCLUDE_DIRECTORIES(
  ${CELLML_INCLUDE_DIR}
  ${CCGS_INCLUDE_DIR}
  ${LIBXML2_INCLUDE_DIR}
)

SET(bsa_SRCS
  src/bsa.cpp
  src/CellMLModelDefinition.cpp
  src/RDFGraph.cpp
  src/SimulationDescription.cpp
  src/ccgs_required_functions.cpp
)

SET(bsaF_SRCS
  src/bsa.f90
  src/CellMLModelDefinitionF.cpp
  src/cellml_model_definition.f90
)

ADD_EXECUTABLE(bsa ${bsa_SRCS})
TARGET_LINK_LIBRARIES(bsa
  ${CELLML_LIBRARIES}
  ${CCGS_LIBRARIES}
  ${Redland_LIBRARIES}
  ${LIBXML2_LIBRARIES}
  -ldl -lm
)

ADD_EXECUTABLE(bsaF ${bsaF_SRCS})
TARGET_LINK_LIBRARIES(bsaF
  -lstdc++
)
SET_TARGET_PROPERTIES(bsaF PROPERTIES LINKER_LANGUAGE Fortran)