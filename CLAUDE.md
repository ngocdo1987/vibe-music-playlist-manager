# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Development
npm run dev          # Start dev server with file watching

# Build & Production
npm run build        # Compile TypeScript for production
npm start            # Run production server

# Testing
npm test             # Run all tests
node ace test tests/functional/hello_world.spec.ts  # Run a single test file

# Database
node ace migration:run         # Run pending migrations
node ace migration:rollback    # Rollback last migration batch

# AdonisJS CLI
node ace make:controller Foo   # Generate a controller
node ace make:model Foo        # Generate a model + migration
node ace make:migration create_foo_table
```

## Architecture

This is an **AdonisJS v5** (TypeScript) full-stack app — an admin-secured music playlist manager with a public-facing MP3 player.

### Request flow
`start/routes.ts` → `app/Middleware/AdminAuth.ts` (for protected routes) → `app/Controllers/Http/` → `app/Models/` → Edge.js views in `resources/views/`

### Route groups (`start/routes.ts`)
- **Public frontend**: `/` and `/p/:slug` handled by `FrontendController`
- **Auth**: `/admin/login` and `/admin/logout` handled by `AuthController`
- **Protected admin** (behind `AdminAuth` middleware): `/admin/*` routes for playlist and song CRUD

### Models (`app/Models/`)
- `Playlist` — has many `Song`s; auto-generates a URL slug from the name using a `beforeSave` hook
- `Song` — belongs to `Playlist`; has a `position` field for drag-drop ordering

### Controllers (`app/Controllers/Http/`)
- `PlaylistController` — full CRUD plus MP3 file upload; validates MIME type with `file-type` package
- `SongController` — delete songs, update titles, reorder positions (PATCH `/admin/songs/:id/position`)
- `AuthController` — login/logout using hardcoded `.env` credentials (`ADMIN_USERNAME` / `ADMIN_PASSWORD`); sets `session.isAdmin`
- `FrontendController` — public home page and per-playlist player page

### Authentication
Session-based. `AdminAuth` middleware (`app/Middleware/AdminAuth.ts`) checks `session.get('isAdmin')` and redirects to login on failure. No user table — credentials come from `.env`.

### File storage
Uploaded MP3s are stored via AdonisJS Drive (configured in `config/drive.ts`). The `Song` model stores both `filename` (UUID-based stored name) and `original_name`.

### Database
SQLite via Lucid ORM. Schema:
- `playlists`: `id`, `name`, `slug`, `description`, timestamps
- `songs`: `id`, `playlist_id`, `title`, `filename`, `original_name`, `position`, timestamps

### TypeScript path aliases (`tsconfig.json`)
`App/*` → `app/*`, `Config/*` → `config/*`, `Contracts/*` → `contracts/*`, `Database/*` → `database/*`

### Testing
Japa test runner with `@japa/preset-adonis`. Functional tests live in `tests/functional/`. Bootstrap is in `tests/bootstrap.ts`. The suite spins up the full AdonisJS application.

### Environment variables
See `.env.example`. Required: `APP_KEY`, `ADMIN_USERNAME`, `ADMIN_PASSWORD`. Validation rules are in `env.ts`.
