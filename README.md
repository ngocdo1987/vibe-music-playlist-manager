# Implementation Plan - C++ Music Manager WebApp

Building a C++ web application with Crow, SQLite, and Bootstrap to manage MP3 playlists.

## Proposed Changes

### Tech Stack
- **Backend Framework**: [Crow (C++ Web Framework)](https://github.com/CrowCpp/Crow)
- **Database**: SQLite3
- **Frontend**: Bootstrap 5, SortableJS (for drag & drop)
- **Environment**: Linux (gcc/g++)

### Database Schema (SQLite)
```sql
CREATE TABLE songs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    filename TEXT NOT NULL
);

CREATE TABLE playlists (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    description TEXT
);

CREATE TABLE playlist_songs (
    playlist_id INTEGER,
    song_id INTEGER,
    position INTEGER,
    PRIMARY KEY (playlist_id, song_id),
    FOREIGN KEY (playlist_id) REFERENCES playlists(id) ON DELETE CASCADE,
    FOREIGN KEY (song_id) REFERENCES songs(id) ON DELETE CASCADE
);
```

### File Structure
- `main.cpp`: Main server logic and routing.
- `db_manager.hpp`: SQLite operations.
- `auth.hpp`: JWT-like or Session-based authentication using `.env`.
- `templates/`:
    - `index.html`: Public playlist list.
    - `player.html`: Public MP3 player.
    - `login.html`: Admin login.
    - `dashboard.html`: Admin dashboard.
    - `edit_playlist.html`: Playlist editor with drag-and-drop.
- `static/`: CSS/JS.
- `mp3/`: MP3 file storage.
- `.env`: Config file for `ADMIN_USER` and `ADMIN_PASS`.

### [NEW] [main.cpp](file:///home/ngoc/projects/pet/cpp/music-manager/main.cpp)
Contains the server logic, routing, and integration of all components.

### [NEW] [db_manager.hpp](file:///home/ngoc/projects/pet/cpp/music-manager/db_manager.hpp)
Handles database connection and queries.

### [NEW] [auth.hpp](file:///home/ngoc/projects/pet/cpp/music-manager/auth.hpp)
Handles session management and `.env` parsing.

## Verification Plan

### Manual Verification
- Access `/admin/dashboard` without login (should redirect).
- Login with credentials from `.env`.
- Create a playlist and upload multiple `.mp3` files.
- Drag and drop to reorder songs.
- Access the public playlist URL and verify the player plays songs in the correct order.
- Test Light/Dark mode toggle.
- Validate that non-MP3 files are rejected.
