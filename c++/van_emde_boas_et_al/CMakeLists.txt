cmake_minimum_required(VERSION 3.10)
project(van_emde_boas_et_al LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")

# Add your main executable
# TODO: make executable.
add_executable(my_tiny_project src/main.cpp lib/my_library.cpp) # Adjust if lib/my_library.cpp is not needed
target_include_directories(my_tiny_project PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Find GTest
find_package(GTest REQUIRED)

# Add your tests
add_executable(my_tiny_project_tests tests/test_my_library.cpp lib/my_library.cpp) # Link your library source again
target_link_libraries(my_tiny_project_tests GTest::gtest_main)
target_include_directories(my_tiny_project_tests PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Enable GTest discovery
include(GoogleTest)
gtest_discover_tests(my_tiny_project_tests)
