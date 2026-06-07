# Repository Guidelines

## Project Structure & Module Organization

This is an AdonisJS 5 TypeScript application for a music playlist manager. Application code lives in `app/`: controllers are under `app/Controllers/Http`, Lucid models are under `app/Models`, middleware is under `app/Middleware`, and exception handling is in `app/Exceptions`. Routes are defined in `start/routes.ts`. Edge templates live in `resources/views`, split into `frontend`, `admin`, `layouts`, and `errors`. Database migrations are in `database/migrations`; model factories are in `database/factories`. Static public assets belong in `public`, with uploaded MP3 files expected under `public/mp3`. Functional tests live in `tests/functional`, with shared test setup in `tests/bootstrap.ts`.

## Build, Test, and Development Commands

- `npm install`: install project dependencies from `package-lock.json`.
- `npm run dev`: start the Adonis development server with file watching via `node ace serve --watch`.
- `npm run build`: compile a production build into `build`.
- `npm start`: run the compiled production server from `server.js`; run `npm run build` first.
- `npm test`: run the Japa test suite through `node ace test`.
- `node ace migration:run`: apply pending database migrations.

## Coding Style & Naming Conventions

Use TypeScript and the path aliases configured in `tsconfig.json`, such as `App/Models/Playlist` and `Config/*`. Follow the existing style: two-space indentation, single quotes, semicolon-free statements, and PascalCase class names. Controllers should use action names such as `index`, `create`, `store`, `edit`, `update`, and `destroy` when implementing CRUD flows. Keep Edge view paths lowercase and grouped by area, for example `resources/views/admin/playlists/edit.edge`.

No formatter or lint script is currently defined in `package.json`; keep changes consistent with nearby files.

## Testing Guidelines

Tests use Japa with the Adonis preset. Add HTTP-level coverage in `tests/functional/*.spec.ts` and name tests after observable behavior, for example `playlist_admin.spec.ts`. Use the API client supplied by the test bootstrap for route assertions. Run `npm test` before submitting changes, and add migration or model coverage when changing database behavior.

## Commit & Pull Request Guidelines

Recent commits use short, imperative messages such as `Add AdonisJS docs` and `Fix wrong Markdown format`. Keep commit subjects concise and focused on one change. Pull requests should include a brief description, test results, linked issues when applicable, and screenshots for UI changes in Edge templates or public/admin pages.

## Security & Configuration Tips

Do not commit secrets, local SQLite databases, or uploaded MP3 files. Keep environment-specific settings in `.env`, and review `config/session.ts`, `config/shield.ts`, and `config/database.ts` when changing authentication, CSRF, or storage behavior.
