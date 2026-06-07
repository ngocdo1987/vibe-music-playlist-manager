# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Music Manager is a Rust web application built with **Actix Web** and **SQLite** for managing MP3 playlists. Despite the repo name containing "NODEJS", this is a pure Rust project.

## Commands

```bash
# Development
cargo run

# Production build
cargo build --release
./target/release/music-manager

# Check for compile errors without building
cargo check

# Linting
cargo clippy

# Format code
cargo fmt
```

## Environment Setup

Copy `.env.example` to `.env` before running:

```bash
cp .env.example .env
```

Required env vars: `ADMIN_USERNAME`, `ADMIN_PASSWORD`, `HOST`, `PORT`, `SESSION_SECRET` (64+ chars). Generate a secret with `openssl rand -hex 32`.

## Architecture

**Shared state** (`AppState` in `src/main.rs`): A single `rusqlite::Connection` wrapped in a `Mutex` is shared across all request handlers via `web::Data<AppState>`. All DB access must lock this mutex. Templates are loaded once at startup via `Tera`.

**Handler modules** (`src/handlers/`):
- `auth.rs` — login/logout, sets `logged_in` in the cookie session
- `admin.rs` — CRUD for playlists (HTML form posts, server-side rendered)
- `api.rs` — JSON endpoints for file upload and playlist song reordering
- `public.rs` — read-only public views (index, playlist player)

**Database** (`src/db.rs`): All SQL is handwritten using `rusqlite`. Three tables: `songs`, `playlists`, `playlist_songs` (join table with `position` for ordering). The DB file is `music.db` (note: `music-manager.db` in the repo root is a stale artifact — `db.rs` opens `music.db`).

**Templates** (`templates/`): Tera templates. Admin templates extend `templates/admin/base.html`; public templates extend `templates/base.html`.

**MP3 storage**: Uploaded files are stored in `mp3/` with UUID-based filenames. The original filename is preserved in the DB. Upload validation checks magic bytes (ID3 tag or frame sync), not just the file extension.

**Authentication**: Session-based via `actix-session` with cookie storage. `is_logged_in()` in `auth.rs` is the guard used by all admin and API handlers — there is no middleware-level auth guard.

**Slug generation**: Playlist slugs are derived from the name and must be unique. Used for public URLs (`/playlist/{slug}`).
