# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A music playlist manager web application written in **Common Lisp** (SBCL). It has a public-facing playlist player and an admin panel for managing playlists and uploading MP3 files. Despite the repo name containing "NODEJS", this branch (`lisp`) is entirely Common Lisp.

## Commands

### Start the server (interactive REPL — preferred for development)
```bash
sbcl
```
Then inside the REPL:
```lisp
(ql:quickload :music-manager)
(music-manager:dev)           ; starts on port 8080
(music-manager:restart-server) ; after code changes
(music-manager:stop-server)
```

### Start via shell script
```bash
./run.sh
```

### Run tests
```lisp
(ql:quickload :music-manager/tests)
(asdf:test-system :music-manager)
```

### Production startup
```bash
sbcl --load start.lisp
```

## Architecture

```
src/
  package.lisp   — namespace & public API exports
  config.lisp    — .env loading, environment variables
  db.lisp        — SQLite connection, schema, query helpers
  models.lisp    — CRUD for playlists, songs, and playlist_songs join table
  auth.lisp      — session auth (admin credentials from .env), flash messages
  upload.lisp    — MP3 validation (extension + magic bytes), file storage
  routes.lisp    — 17 HTTP endpoints (easy-routes + Hunchentoot)
  main.lisp      — server startup, static file dispatchers

templates/        — Djula (Jinja2-like) HTML templates
  admin/          — login, dashboard, playlist form
  public/         — player, welcome, 404

static/
  js/player.js    — vanilla JS audio player (click-to-play, auto-next)
  js/playlist.js  — drag-drop reorder (SortableJS), multi-file MP3 upload
  js/theme.js     — Bootstrap 5 light/dark mode, persisted in localStorage
```

### Database schema (SQLite)

```sql
playlists      (id, name, created_at)
songs          (id, filename, original_name, created_at)
playlist_songs (playlist_id, song_id, position)   -- many-to-many, cascade delete
```

### Request flow

Public routes (`/`, `/playlist/:id`) — no auth required, server-side rendered via Djula.

Admin routes (`/admin/*`) — session-checked in `auth.lisp`; redirected to `/admin/login` if unauthenticated.

Upload (`POST /admin/upload`) and song ordering (`POST /admin/playlist/:id/order`) return JSON; all other admin routes return HTML redirects.

MP3 files are stored in `mp3/` and served as static files under `/mp3/`.

## Configuration

Copy `.env.example` → `.env`. Required keys: `ADMIN_USER`, `ADMIN_PASSWORD`, `SECRET_KEY`. Optional: `PORT` (default 8080), `DB_PATH` (default `music-manager.db`).

## Dependencies (ASDF system)

Defined in `music-manager.asd`. Key libraries: `hunchentoot` (HTTP server), `easy-routes` (routing), `djula` (templates), `cl-dbi` + `dbd-sqlite3` (database), `cl-dotenv` (env vars), `cl-json`, `alexandria`, `uiop`.

Quicklisp manages all dependencies. Link the project into Quicklisp local-projects before first load:
```bash
ln -s $(pwd) ~/.quicklisp/local-projects/music-manager
```

## Known Issues

See `problems.md` (Vietnamese): upload success/failure UI feedback is unreliable; the song list in the admin panel doesn't auto-refresh after upload — users must reload manually.
