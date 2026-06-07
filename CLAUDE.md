# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **pure Zig web application** for managing MP3 music playlists, despite the `NODEJS` directory name. The `master` branch holds the original Node.js/Express implementation; the current branch (`zig-purezig`) is a full rewrite using only Zig 0.15.x stdlib — no web framework, no ORM.

The repository contains multiple branches, each a separate implementation of the same app in a different language/framework (Rust/Actix, C++/Crow, Bun, AdonisJS, Lisp, Fortran, etc.).

## Build & Run

```bash
# Build release binary to zig-out/bin/music-manager
zig build

# Build and run immediately
zig build run

# Run tests for individual modules
zig test src/db.zig
zig test src/router.zig
zig test src/utils/mp3.zig
zig test src/utils/multipart.zig
```

**Docker:**
```bash
docker-compose up -d
```

## Environment Configuration

Copy `.env.example` to `.env` before running. All three variables are required — the code has no defaults:

```
ADMIN_USERNAME=admin
ADMIN_PASSWORD=changeme123
SERVER_PORT=8080
```

`main.zig` does a `chdir` to project root on startup so `.env` and `music_manager.db` resolve correctly regardless of where the binary is executed from.

## Architecture

### Request Lifecycle

Single-threaded TCP listener (`main.zig`) → `router.zig` parses raw HTTP → route dispatch by path prefix → handler reads from `db.zig` or `auth.zig` → template function in `templates/` writes HTML directly into response buffer → response flushed to TCP connection.

**Global state:** Two module-level pointers — `g_database` and `g_auth` — hold the SQLite connection and auth config for the lifetime of the process.

### Key Source Files

| File | Responsibility |
|------|---------------|
| `src/main.zig` | TCP listener, connection loop, entry point |
| `src/router.zig` | HTTP request/response structs, method/path/header parsing, route dispatch |
| `src/db.zig` | SQLite CRUD via C API bindings (`@cImport`) |
| `src/auth.zig` | `.env` loading, credential storage |
| `src/templates/layout.zig` | Base HTML shell (Bootstrap 5, theme toggle) |
| `src/templates/public.zig` | Public homepage and playlist player |
| `src/templates/admin.zig` | Admin login form, dashboard, song/playlist management |
| `src/utils/session.zig` | In-memory session store (32-byte random tokens, 24h TTL) |
| `src/utils/multipart.zig` | Multipart form boundary parsing for file uploads |
| `src/utils/mp3.zig` | Magic byte validation (ID3v2 or 0xFF 0xFB frame sync) |

### Database Schema

Three SQLite tables with cascading deletes:
- **playlists** — `id`, `name`, `slug` (unique), `created_at`, `updated_at`
- **songs** — `id`, `title`, `filename` (unique), `file_path`, `created_at`
- **playlist_songs** (junction) — `playlist_id`, `song_id`, `position`

### HTML Rendering

All HTML is generated procedurally by Zig functions writing string literals into response buffers. There is no template engine. Response body uses a fixed 1MB buffer; headers use a fixed 4KB buffer — avoid approaches that exceed these limits.

### MP3 Upload Flow

Multipart body parsed in `utils/multipart.zig` → file bytes validated against magic bytes in `utils/mp3.zig` → saved with a timestamp-based unique filename to `public/mp3/` → path recorded in `songs` table.

### Authentication

Credentials loaded from `.env` at startup. Login sets a session cookie validated on every `/admin/*` request via the in-memory session store in `utils/session.zig`.

## C Interop Note

`db.zig` uses `@cImport` to bind the SQLite3 C API. The build links against the system `sqlite3` library (`-lsqlite3`). Any new C library usage follows the same `@cImport` / `addSystemFramework` pattern in `build.zig`.
