# Repository Guidelines

## Project Structure & Module Organization
This repository contains a Fortran FastCGI music playlist manager. The main entry point is `fortran_fcgi.f90`, which handles request routing and response dispatch. Core modules live at the repository root: `db_module.f90` for SQLite access, `auth_module.f90` for sessions, `env_module.f90` for `.env` parsing, `mp3_module.f90` for upload validation, and `template_module.f90` for HTML rendering. Templates are in `template/`, with reusable fragments in `template/partials/`. Browser assets live in `static/css/` and `static/js/`. Uploaded media belongs in `mp3/` or `public/mp3/`; do not commit user uploads. Runtime files such as `music.db`, `crash.log`, and `debug_upload.log` should remain local.

## Build, Test, and Development Commands
- `make`: compiles the production `fortran_fcgi` binary with `gfortran -O2 -Wall`.
- `make debug`: rebuilds with runtime checks, backtraces, and debug symbols.
- `make clean`: removes generated objects, module files, and the binary.
- `./restart.sh`: recompiles and starts FastCGI on `127.0.0.1:9000` for local development.
- `./debug.sh`: starts a debug FastCGI process and writes crash output to `crash.log`; update its hard-coded project path before use if needed.

## Coding Style & Naming Conventions
Use free-form Fortran with 4-space indentation, `implicit none`, and small module procedures where practical. Keep module names descriptive and lowercase with `_module` suffixes. Prefer explicit C bindings and constants near the interfaces they support. Use clear template names matching routes or components, such as `admin_dashboard.html` and `partials/song_item.html`. Keep JavaScript behavior separated by page concern in `static/js/`.

## Testing Guidelines
There is no formal automated test suite yet. Before submitting changes, run `make` and, for risky Fortran changes, `make debug`. Manually verify public playlist browsing, admin login, playlist CRUD, MP3 upload validation, song ordering, and audio playback. When adding tests, use focused scripts or fixtures with names that describe the behavior under test.

## Commit & Pull Request Guidelines
Current history uses short, imperative commit messages such as `Add AdonisJS docs` and `Fix wrong Markdown format`. Follow that style: concise subject, capitalized first word, no trailing period. Pull requests should describe the user-visible change, list manual verification steps, note configuration or service changes, and include screenshots for template or asset updates.

## Security & Configuration Tips
Do not commit real `.env` credentials, generated databases, uploaded MP3s, or logs. Change the default admin password before deployment. Keep Nginx, systemd, and FastCGI paths consistent when moving the project between machines.


<claude-mem-context>
# Memory Context

# [music-playlist-manager-dev] recent context, 2026-06-07 4:39am UTC

No previous sessions found.
</claude-mem-context>