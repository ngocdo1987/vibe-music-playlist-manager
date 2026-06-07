# Repository Guidelines

## Project Structure & Module Organization

This repository is a Common Lisp web application for MP3 playlist management. The ASDF system is defined in `music-manager.asd`, with source files loaded from `src/` in dependency order: package setup, configuration, database access, models, auth, upload handling, routes, and server startup. Tests live in `tests/`, currently as a FiveAM suite in `tests/main-test.lisp`. HTML templates are split between `templates/public/` and `templates/admin/`, with shared layout files under `templates/`. Static browser assets live in `static/css/` and `static/js/`. Uploaded music belongs in `mp3/`; keep media files out of git.

## Build, Test, and Development Commands

- `sbcl --eval "(ql:quickload :music-manager)" --eval "(music-manager:start-server)"`: load the app and start the server.
- `./run.sh`: runs the same startup command; make it executable if needed with `chmod +x run.sh`.
- `sbcl --eval "(ql:quickload :music-manager)" --eval "(asdf:test-system :music-manager)" --quit`: run the FiveAM test system.
- In a REPL, use `(music-manager:dev)`, `(music-manager:restart-server)`, and `(music-manager:stop-server)` for iterative development.

Install SBCL, Quicklisp, SQLite, and the Quicklisp dependencies declared in `music-manager.asd` before running locally.

## Coding Style & Naming Conventions

Use idiomatic Common Lisp formatting with two-space indentation inside forms where practical. Keep functions and variables in kebab-case, predicates ending in `-p`, and package-private helpers internal unless they must be exported. Add source files to `music-manager.asd` in load order. Keep route handlers in `src/routes.lisp`, persistence logic in `src/models.lisp` or `src/db.lisp`, and upload validation in `src/upload.lisp`. For frontend files, use descriptive lowercase names such as `playlist.js` and keep Bootstrap-oriented markup in templates.

## Testing Guidelines

Tests use FiveAM under the `music-manager/tests` system. Name tests after the behavior under test, for example `mp3-extension-valid` or `config-parse-env-line`. Prefer small unit tests for validation, parsing, and model behavior; use temporary files and `unwind-protect` when a test writes to disk. Run the full suite with `asdf:test-system` before opening a PR.

## Commit & Pull Request Guidelines

The current history uses short imperative commit messages such as `Fix wrong Markdown format` and `Add AdonisJS docs`. Follow that style: concise, present-tense, and focused on one change. Pull requests should include a brief description, test results, linked issues when applicable, and screenshots for template or styling changes.

## Security & Configuration Tips

Do not commit `.env`, SQLite databases, uploaded MP3s, or session secrets. Configure admin credentials, port, host, database path, and `SESSION_SECRET` through environment variables or a local `.env` file.
