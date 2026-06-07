# Repository Guidelines

## Project Structure & Module Organization

This is a small C++ music playlist web app built with Crow, SQLite, and Mustache templates.

- `main.cpp` defines the Crow app, public playlist routes, admin routes, upload handling, and server startup.
- `db_manager.hpp` owns SQLite setup, schema initialization, and playlist/song queries.
- `auth.hpp` loads `.env` values and validates admin credentials.
- `templates/` contains Mustache views such as `index.html`, `login.html`, `dashboard.html`, and player screens.
- `docs/adonisjs/` is historical reference material, not current implementation source.
- Runtime files such as `.env`, `music-manager.db`, `mp3/`, and uploaded media should stay out of commits unless explicitly required.

## Build, Test, and Development Commands

There is no committed build system yet. Use a direct compiler command while developing:

```bash
g++ -std=c++17 main.cpp -lsqlite3 -lpthread -o music-manager
./music-manager
```

Create local config from the example:

```bash
cp .env.example .env
```

Set `ADMIN_USER`, `ADMIN_PASS`, optional `PORT`, and optional `DB_PATH`. If `PORT` is empty, the app defaults to `8080`; `.env.example` currently uses `3534`.

## Coding Style & Naming Conventions

Use C++17. Follow the existing style: 4-space indentation, same-line opening braces for functions and route handlers, `snake_case` for helpers and local variables, and PascalCase for classes such as `DBManager` and `AuthManager`. Keep route-specific logic near its route, and use prepared SQLite statements for user-controlled input.

## Testing Guidelines

No automated tests are currently present. Before submitting changes, compile the app and manually verify core flows:

- Visit `/` and `/playlist/<id>`.
- Confirm `/admin/dashboard` redirects when unauthenticated.
- Log in through `/admin/login` using `.env` credentials.
- Create a playlist, upload only `.mp3` files, reorder songs, and verify playback order.

If adding tests, prefer small C++ tests for `DBManager` with a temporary SQLite database. Name test files `*_test.cpp`.

## Commit & Pull Request Guidelines

Recent commits use short imperative summaries, for example `Fix wrong Markdown format` and `Add AdonisJS docs`. Keep commit subjects concise and action-oriented.

Pull requests should include a brief description, manual verification steps, config or migration notes, and screenshots for template/UI changes. Link related issues when available and call out changes to `.env.example`, database schema, or uploaded file handling.

## Security & Configuration Tips

Do not commit real `.env` credentials, uploaded MP3s, or local database files. Validate upload paths and filenames carefully; `sanitize_filename` is the current guard for filesystem writes.
