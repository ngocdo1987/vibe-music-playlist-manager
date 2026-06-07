# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Development (auto-reload via Bun's built-in watcher)
bun run dev

# Production
bun run start

# Install dependencies
bun install
```

No test suite or linter is configured. There is no build step — Bun runs source files directly.

## Environment Variables

A `.env` file is required at the project root. Required variables:

```
PORT=3000
ADMIN_USERNAME=
ADMIN_PASSWORD=
SESSION_SECRET=
NODE_ENV=development
MP3_PATH=/absolute/path/to/mp3   # absolute path to where uploaded MP3s are stored
```

`MP3_PATH` is used directly for file write and delete operations in the admin routes. If unset, uploads and deletes will break.

## Architecture

**Runtime**: Bun (not Node.js). The SQLite database uses `bun:sqlite` (a Bun built-in), so this app cannot run under Node.js without replacing `src/config/database.js`.

**Stack**: Express.js + EJS templates + Bun's native SQLite + express-fileupload. No ORM — all queries use `db.prepare(...).run/get/all()` directly.

**Entry point**: `src/app.js` — wires middleware, mounts routes, starts the server.

### Route structure

| Prefix | File | Auth |
|--------|------|------|
| `/admin` | `src/routes/admin.js` | Session-based (`req.session.isAdmin`) |
| `/` | `src/routes/public.js` | None |

Admin login compares plaintext credentials from env vars — no password hashing despite `bcryptjs` being listed as a dependency.

### Database schema

Three tables in `database.sqlite` (auto-created on first run by `src/config/database.js`):

- `playlists` — id, name, description, slug (unique), timestamps
- `songs` — id, filename (stored name), original_name, file_path, file_size, duration
- `playlist_songs` — junction table with `position` for drag-and-drop ordering; cascade-deletes when a playlist is deleted

Songs are shared across playlists. When a song is removed from its last playlist, the file on disk and the `songs` row are both deleted.

### File storage

Uploaded MP3s go to the path in `MP3_PATH`. The `file_path` column in `songs` stores `mp3/<filename>` (relative), but deletion in `admin.js` uses `MP3_PATH + '/' + filename` (absolute via env var). The two approaches are inconsistent — the remove-song route uses the relative `file_path` joined with `__dirname`, while the delete-playlist route uses `MP3_PATH`. Keep this in mind when modifying upload/delete logic.

### Views

EJS templates in `src/views/`:
- `admin/login.ejs`, `admin/dashboard.ejs`, `admin/playlist-form.ejs`
- `public/player.ejs` — home page listing all playlists
- `public/player-winamp.ejs` — Winamp-style player rendered for `/playlist/:slug`

Static assets served from `/public` (maps to `public/` dir). MP3 files served from `/mp3` (maps to the `mp3/` dir relative to project root, distinct from `MP3_PATH`).
