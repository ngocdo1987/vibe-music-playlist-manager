# Repository Guidelines

## Project Structure & Module Organization

This is a Node.js Express application for managing and playing music playlists. The entry point is `src/app.js`, which configures middleware, sessions, static file serving, routes, and the EJS view engine.

- `src/routes/`: route handlers split into `admin.js` and `public.js`.
- `src/middleware/`: shared Express middleware, currently authentication.
- `src/config/database.js`: SQLite setup using `better-sqlite3`; tables are created on startup.
- `src/views/admin/` and `src/views/public/`: EJS templates for the admin UI and public player.
- `docs/adonisjs/`: reference documentation for a possible AdonisJS migration.
- Runtime data lives outside source: `database.sqlite` is created by the app, and uploaded MP3s go in `mp3/`. These should not be treated as source assets.

## Build, Test, and Development Commands

- `npm install`: install Express, EJS, SQLite, upload, session, and development dependencies.
- `npm run dev`: start the app through `nodemon` for local development.
- `npm start`: run `node src/app.js` for production-style execution.

The app listens on `PORT` from `.env`, defaulting to `3000`. Admin login is at `/admin/login`; the public player starts at `/`.

## Coding Style & Naming Conventions

Use CommonJS modules (`require`, `module.exports`) and keep route files focused by feature. Follow the existing two-space indentation and concise handler style. Prefer clear lowercase file names such as `admin.js`, `public.js`, and `database.js`. Use descriptive route paths (`/playlist/create`, `/song/upload`) and keep database column names in `snake_case` to match the existing schema.

## Testing Guidelines

No automated test script is currently configured in `package.json`. When adding tests, include an `npm test` script and place tests near the behavior they cover or under a new `tests/` directory. Prioritize route behavior, authentication, upload validation, playlist ordering, and SQLite persistence. Until tests exist, manually verify `npm run dev`, admin login, playlist creation/editing, MP3 upload, and public playback.

## Commit & Pull Request Guidelines

Recent commits use short imperative or descriptive messages, for example `Add AdonisJS docs` and `Fix wrong Markdown format`. Keep commits focused and avoid mixing generated runtime files with source changes.

Pull requests should include a brief summary, the commands or manual checks performed, linked issues when applicable, and screenshots for UI changes in EJS templates or public/admin player behavior.

## Security & Configuration Tips

Keep `.env` secrets out of commits. Set `ADMIN_USERNAME`, `ADMIN_PASSWORD`, and a strong `SESSION_SECRET` before running the app. Do not commit uploaded MP3 files or local SQLite databases unless a specific fixture is intentionally added.
