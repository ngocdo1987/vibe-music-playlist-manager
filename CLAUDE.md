# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
npm run dev      # Start with nodemon (auto-reload)
npm start        # Production start (node src/app.js)
```

No build step, no test runner, no linter configured.

**First-time setup**: copy `.env.example` to `.env` and set `ADMIN_USERNAME`, `ADMIN_PASSWORD`, and `SESSION_SECRET`. The SQLite database and `/mp3` directory are created automatically on first start.

## Architecture

Express.js + EJS + SQLite app. No ORM — raw SQL via `better-sqlite3`.

**Entry point**: `src/app.js` wires middleware and mounts two routers:
- `src/routes/admin.js` — all `/admin/*` routes (CRUD for playlists, MP3 upload, song reordering)
- `src/routes/public.js` — `/` (playlist index) and `/playlist/:slug` (player view)

**Auth**: `src/middleware/auth.js` checks `req.session.isAdmin`. Admin credentials live in `.env` and are compared **as plaintext** — `bcryptjs` is installed but not used.

**Database**: `src/config/database.js` initializes three tables on startup:
- `playlists` (id, name, slug, description)
- `songs` (id, filename, original_name, file_path, file_size, duration)
- `playlist_songs` junction table with a `position` column for drag-and-drop ordering; cascade deletes enabled

**Views**: EJS templates in `src/views/admin/` (login, dashboard, playlist-form) and `src/views/public/` (player, player-winamp). Bootstrap 5.3 + Bootstrap Icons via CDN.

**Static files**: `public/` served at `/public`, uploaded MP3s stored in `/mp3/` and served at `/mp3`.

**File uploads**: `express-fileupload` with a 50 MB limit; files are saved with a `{timestamp}_{originalname}` prefix.

## Key Details

- Port defaults to `3535` (set via `PORT` in `.env`)
- Session cookie expires in 24 hours; marked `secure: true` only when `NODE_ENV=production`
- The `duration` column exists in `songs` but is never populated (always `null`)
- `docs/adonisjs/` contains AdonisJS reference docs unrelated to this project
