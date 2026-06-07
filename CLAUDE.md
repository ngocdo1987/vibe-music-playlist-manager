# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a **Fortran** web application (not Node.js — the directory name is historical). It serves a music playlist manager via FastCGI + Nginx with a SQLite backend. Despite the `.f90` extension, all application logic is Fortran 2003/2008 using ISO_C_BINDING to call C libraries directly.

## Build & Run

```bash
# Build (release)
make

# Build with runtime checks and backtraces (for debugging crashes)
make debug

# Clean build artifacts
make clean

# Recompile and restart the FastCGI server (for development)
./restart.sh
```

**Dependencies required**: `gfortran`, `libfcgi-dev`, `libsqlite3-dev`, `spawn-fcgi`, `nginx`

Install on Ubuntu/Debian:
```bash
sudo apt-get install -y gfortran libfcgi-dev libsqlite3-dev nginx spawn-fcgi
```

**Run manually** (from project directory — working directory matters for relative paths):
```bash
spawn-fcgi -a 127.0.0.1 -p 9000 -d "$(pwd)" ./fortran_fcgi
```

**View logs**:
```bash
sudo tail -f /var/log/nginx/music-manager-error.log
# Upload debug log (written by the app itself):
cat debug_upload.log
```

**Reset the database**:
```bash
rm music.db && ./restart.sh
```

## Architecture

The build order (declared in `Makefile`) reflects the dependency chain:

```
env_module.f90 → db_module.f90 → auth_module.f90
mp3_module.f90 (independent)
template_module.f90 (independent)
fortran_fcgi.f90 (main program, uses all modules)
```

### Module responsibilities

| File | Role |
|------|------|
| `fortran_fcgi.f90` | Main program: FCGX request loop, URL router (`route_request`), all HTTP handler subroutines, multipart upload parser |
| `db_module.f90` | SQLite C-binding interface; `playlist_t` and `song_t` types; all CRUD operations on three tables: `playlists`, `songs`, `playlist_songs` |
| `auth_module.f90` | Single in-memory session token; reads credentials from `env_module`; `validate_login` / `create_session` / `check_session` |
| `env_module.f90` | Parses `.env` file at startup; exposes `admin_user`, `admin_pass`, `session_secret` as module-level variables |
| `mp3_module.f90` | Validates uploaded files by checking ID3 tags or MP3 frame sync bytes before adding to DB |
| `template_module.f90` | File-based template engine: reads `.html` files, replaces `{{variable}}` placeholders via `template_set` / `template_render` |

### Key constraints to be aware of

- **Single session only**: `auth_module` stores exactly one active token in a `save` variable. A new login invalidates the previous session.
- **Static array limits**: Playlist/song result sets are capped at 100 entries (`type(playlist_t), save :: playlists(100)`).
- **Template limits**: Max 50 variables (`MAX_VARS`), values up to 4096 chars, total template output up to 65536 chars.
- **POST body limit**: Non-multipart POST data is read into an 8192-char buffer. Multipart (file upload) is handled byte-by-byte directly from the FCGX stream.
- **Working directory**: The app uses relative paths for `music.db`, `template/`, and `public/mp3/`. The process must start from the project directory (handled by `restart.sh`).
- **Uploaded filenames**: The upload handler ignores the browser-supplied filename and generates a timestamp-based name (e.g. `upload_20260607_123456_0001.mp3`) to avoid UTF-8 issues. The original filename is used only as the song title.

### URL routing

All routing is in `route_request` in `fortran_fcgi.f90`:

- `GET /` → public playlist index
- `GET /playlist/:id` → public audio player
- `GET/POST /admin/login` → login form / credential check
- `GET /admin/logout` → destroy session
- `GET/POST /admin/playlist/new` → create playlist
- `GET/POST /admin/playlist/:id/edit` → edit playlist + song list
- `POST /admin/playlist/:id/delete` → delete playlist
- `POST /admin/playlist/:id/reorder` → reorder songs (returns JSON `{"status":"ok"}`)
- `POST /admin/upload` → multipart file upload
- `GET /admin/song/:id/remove/:playlist_id` → remove song from playlist

### Static files & MP3 serving

Nginx serves `static/` directly and proxies everything else to the FastCGI process on port 9000. Uploaded MP3s are written to `public/mp3/` and served by Nginx at `/mp3/` paths. The DB stores the web path (e.g. `/mp3/upload_....mp3`).

### Configuration

Credentials and session secret live in `.env` (see `.env.example`). The file is parsed once at startup by `load_env()`.
