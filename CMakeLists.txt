cmake_minimum_required(VERSION 3.5)
project(MusicPlaylistManager CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")

# Find Drogon
find_package(Drogon CONFIG REQUIRED)

# Include directories
include_directories(${PROJECT_SOURCE_DIR})

# Source files
set(SOURCES
    main.cc
    controllers/PlaylistController.cc
    controllers/AuthController.cc
    models/Song.cc
    models/Playlist.cc
    filters/AuthFilter.cc
)

# Create executable
add_executable(${PROJECT_NAME} ${SOURCES})

# Link libraries
target_link_libraries(${PROJECT_NAME} PRIVATE Drogon::Drogon)

# Copy views to build directory
file(GLOB VIEW_FILES "${CMAKE_CURRENT_SOURCE_DIR}/views/*.csp")
file(COPY ${VIEW_FILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/views)

# Copy public folder to build directory
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/public)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/public DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()

# Copy config.json to build directory
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/config.json)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/config.json DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()

# Copy database.sql to build directory
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database.sql)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/database.sql DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()

# Copy .env to build directory
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.env)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/.env DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()

# Create necessary directories
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mp3)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/logs)