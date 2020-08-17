cmake_minimum_required(VERSION 3.17)
project(objective_go)
include(FindPkgConfig)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)

if(${CMAKE_BUILD_TYPE} MATCHES "Release")
    add_compile_options(-flto=full)
endif()

add_compile_options(-DGC_THREADS)
add_compile_options(-D_BSD_SOURCE=1)
add_compile_options(-D_DARWIN_C_SOURCE=1)
add_compile_options(-D__OS_${CMAKE_SYSTEM_NAME}__=1)

pkg_check_modules(FMT REQUIRED fmt)
pkg_check_modules(BDWGC REQUIRED bdw-gc)

link_directories(
    ${FMT_LIBRARY_DIRS}
    ${BDWGC_LIBRARY_DIRS}
)

include_directories(
    ${CMAKE_SOURCE_DIR}
    ${FMT_INCLUDE_DIRS}
    ${BDWGC_INCLUDE_DIRS}
)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmodules-ts")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -include gc/gc_cpp.h")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -include gc/gc_allocator.h")

set(SOURCES
    main.cpp
    tokenizer/tokenizer.cpp
    tokenizer/tokenizer.h
    utils/traits.h
    tokenizer/token.cpp
    tokenizer/token.h
    utils/format.h
    utils/types.h
    utils/repr.cpp
    utils/repr.h)

add_executable(objective_go ${SOURCES})
target_link_libraries(objective_go ${FMT_LIBRARIES} ${BDWGC_LIBRARIES} -lgccpp)
