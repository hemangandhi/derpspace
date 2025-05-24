cmake_minimum_required(VERSION 3.10)
project(van_emde_boas_et_al LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")

# Find GTest
find_package(GTest REQUIRED)

# Add your tests
add_executable(van_emde_boas_et_al_tests tests/test_my_library.cpp)
target_link_libraries(van_emde_boas_et_al_tests GTest::gtest_main)
target_include_directories(van_emde_boas_et_al_tests PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Enable GTest discovery
include(GoogleTest)
gtest_discover_tests(van_emde_boas_et_al_tests)
