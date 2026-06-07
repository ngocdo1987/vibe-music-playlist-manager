# Repository Guidelines

## Project Structure & Module Organization

This repository is a Bun-powered Express app for managing music playlists. The main entry point is `src/app.js`, which sets up sessions, file uploads, static routes, EJS views, and route modules.

- `src/routes/admin.js`: admin login, playlist CRUD, MP3 uploads, and song ordering.
- `src/routes/public.js`: public playlist listing and player pages.
- `src/config/database.js`: Bun SQLite initialization and schema creation.
- `src/middleware/auth.js`: admin access guard.
- `src/views/admin/` and `src/views/public/`: EJS templates for the admin panel and public player.
- `docs/adonisjs/`: migration/reference notes, not runtime code.

Runtime data is created outside source: `database.sqlite` stores local data, and MP3 files are stored under the configured upload path.

## Build, Test, and Development Commands

- `bun install`: install dependencies from `package.json` and `bun.lock`.
- `bun run dev`: run `bun --watch src/app.js` for local development with reloads.
- `bun run start`: run the production-style server command.
- `bun --version`: confirm Bun v1.0 or newer is available.

The app defaults to port `3000`. Visit `/` for the public player and `/admin/login` for administration.

## Coding Style & Naming Conventions

Use CommonJS modules (`require`, `module.exports`) to match the current code. Keep two-space indentation, concise Express handlers, and lowercase file names such as `admin.js` and `database.js`. Use `snake_case` for SQLite columns and clear route paths such as `/playlist/create` or `/song/upload`. Keep EJS page-specific scripts close to their templates unless shared assets are introduced.

## Testing Guidelines

No automated test script is currently defined. When adding tests, add a `test` script to `package.json` and prefer Bun-compatible tests for route behavior, auth redirects, upload validation, playlist ordering, and SQLite persistence. Name test files by feature, for example `admin-routes.test.js`. Until coverage exists, manually verify `bun run dev`, login, playlist create/edit/delete, MP3 upload, reorder, and public playback.

## Commit & Pull Request Guidelines

Recent history uses short imperative or descriptive messages such as `Customize for Bun`, `Add MP3_PATH env`, and `Fix wrong Markdown format`. Keep commits focused and avoid committing generated databases, uploaded MP3 files, or local secrets.

Pull requests should include a short summary, commands or manual checks run, linked issues when relevant, and screenshots for admin or player UI changes.

## Security & Configuration Tips

Create a local `.env` with `ADMIN_USERNAME`, `ADMIN_PASSWORD`, `SESSION_SECRET`, and `MP3_PATH`. Use a strong session secret and ensure `MP3_PATH` exists and is writable by the app. Do not commit `.env`, uploaded music, or SQLite runtime databases.
