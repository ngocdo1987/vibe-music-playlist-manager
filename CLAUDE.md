# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ASP.NET Core 9 MVC web application for managing and publicly serving MP3 playlists. Single-admin model: one admin manages playlists/songs, public users browse and listen without authentication.

## Commands

```bash
# Restore dependencies
dotnet restore

# Build (development)
dotnet build

# Build (production)
dotnet build --configuration Release
# or: ./build.sh

# Run development server (http://localhost:5000, https://localhost:5001)
dotnet run
# or: ./run.sh

# Database migrations
dotnet ef migrations add <MigrationName>
dotnet ef database update
```

**No automated test suite** — testing is manual.

## Environment Setup

Copy `.env.example` to `.env` and set credentials before running:
```
ADMIN_USERNAME=your_username
ADMIN_PASSWORD=your_password
```

The `.env` file is loaded by `DotNetEnv` in `Program.cs`. SQLite database (`music.db`) is auto-created on first run.

## Architecture

### Request Flow

- **Public routes** (`/`, `/public/*`): `PublicController` — no auth required
- **Admin routes** (`/admin/*`, `/playlistadmin/*`): Protected by `AdminAuthMiddleware`, which checks session on every request
- **Auth**: `AuthController` handles login/logout; `AuthService` validates credentials against `.env` values

### Data Model

Three EF Core entities in `Models/`:
- `Song` — MP3 file metadata (title, artist, filename, path, size)
- `Playlist` — named container with optional description
- `PlaylistSong` — junction table with `Order` field for drag-drop sequencing

`Data/MusicDbContext.cs` configures the relationships and is the single EF context.

### File Uploads

`Services/FileService.cs` validates (extension + MIME type) and stores MP3s to `wwwroot/mp3/` with GUID-based filenames. Size limit (default 50 MB) is in `appsettings.json` under `FileUpload:MaxFileSizeMB`.

### Frontend

- Bootstrap 5 + vanilla JS — no build step, no bundler
- `wwwroot/js/playlist-editor.js` — drag-drop reorder (posts new order via fetch)
- `wwwroot/js/theme-toggle.js` — light/dark theme persisted in `localStorage`
- `Views/Public/Player.cshtml` — HTML5 `<audio>` player with auto-advance

### Key Configuration

- `appsettings.json` — SQLite connection string (`music.db`), file upload limits, logging
- `Program.cs` — service registration, middleware pipeline order (auth middleware registered before MVC)
