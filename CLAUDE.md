# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository implements a **music playlist manager web app** across multiple technology stacks, each on its own git branch. The current branch (`cpp-crow`) uses C++ with the Crow web framework and SQLite3.

| Branch | Stack |
|---|---|
| `cpp-crow` | C++ + Crow + SQLite3 |
| `cpp-drogon` | C++ + Drogon + SQLite3 |
| `node-expressjs` | Node.js + Express |
| `node-adonisjs` | Node.js + AdonisJS |
| `bun-expressjs` | Bun + Express |
| `rust-actix-web` | Rust + Actix-web |
| `csharp-aspnet` | C# + ASP.NET |
| `zig-purezig` | Zig |
| `fortran` | Fortran |
| `lisp` | Lisp |

## Build & Run (cpp-crow branch)

**Build:**
```bash
g++ -std=c++17 -O2 main.cpp -o music_manager \
  -I/usr/local/include \
  -lsqlite3 -lpthread
```

**Run:**
```bash
cp .env.example .env  # then fill in ADMIN_USER and ADMIN_PASS
mkdir -p mp3 static
./music_manager
```

The server reads `PORT` from `.env` (default `3534`). Crow's Mustache template engine looks for templates in `./templates/` relative to the working directory.

## Architecture (cpp-crow)

All server logic lives in `main.cpp` — a single-file Crow app using `crow::SimpleApp`. Routes are registered as lambdas capturing `db` (DBManager) and `is_authenticated` (a lambda).

**`db_manager.hpp`** — `DBManager` class wraps raw SQLite3 prepared statements. Tables (`songs`, `playlists`, `playlist_songs`) are auto-created on first run via `init_tables()`. A song can belong to multiple playlists; orphaned songs (no remaining playlist memberships) are deleted along with their `mp3/` files when a playlist is removed.

**`auth.hpp`** — `AuthManager` parses `.env` as `KEY=value` pairs. Authentication is a plain cookie: login sets `session=admin_logged_in`; the `is_authenticated` lambda in `main.cpp` checks for that exact cookie string. No token signing — the session is only as secure as the cookie transport.

**`templates/`** — Crow's built-in Mustache engine via `crow::mustache::load("file.html").render(ctx)`. Template directory is resolved relative to CWD at runtime.

**MP3 handling** — Files are uploaded via multipart POST to `/admin/upload/<playlist_id>`. Filenames are sanitized (alphanumeric, `.`, `_`, `-` only) before writing to `mp3/`. The original filename is stored as the song title; the sanitized name is used for the on-disk path.

## .env Configuration

```
ADMIN_USER=<username>
ADMIN_PASS=<password>
PORT=3534
DB_PATH=          # defaults to music.db if blank
```

## Key Route Map

| Method | Path | Description |
|---|---|---|
| GET | `/` | Public playlist index |
| GET | `/playlist/<id>` | Public Winamp-style player |
| GET/POST | `/admin/login` | Admin login |
| GET | `/admin/dashboard` | Admin playlist list |
| POST | `/admin/playlist/new` | Create playlist |
| GET | `/admin/playlist/edit/<id>` | Edit playlist (drag-and-drop reorder) |
| POST | `/admin/upload/<id>` | Upload MP3 to playlist |
| POST | `/admin/playlist/save_order/<id>` | Persist drag-and-drop order (JSON body `{song_ids:[...]}`) |
| GET | `/admin/playlist/delete/<id>` | Delete playlist + orphaned files |
| GET | `/admin/logout` | Clear session cookie |
| GET | `/assets/<path>` | Static files from `static/` |
| GET | `/mp3/<path>` | MP3 files from `mp3/` |
