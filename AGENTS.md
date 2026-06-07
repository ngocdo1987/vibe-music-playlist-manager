# Repository Guidelines

## Project Structure & Module Organization

This is an ASP.NET Core 9 MVC music playlist manager backed by SQLite. Request handling lives in `Controllers/`, custom admin authentication is in `Middleware/`, EF Core entities are in `Models/`, and the database context is `Data/MusicDbContext.cs`. Generated migrations belong in `Migrations/`. Put business logic in `Services/` so controllers stay focused on HTTP flow.

Razor views are organized by controller under `Views/`, with shared layouts in `Views/Shared/`. Static assets are in `wwwroot/`: CSS in `wwwroot/css/`, JavaScript in `wwwroot/js/`, and runtime MP3 uploads in `wwwroot/mp3/`.

## Build, Test, and Development Commands

- `dotnet restore`: restore NuGet dependencies.
- `dotnet run` or `./run.sh`: start the local development server.
- `dotnet build`: compile the project.
- `dotnet build --configuration Release` or `./build.sh`: create a release build.
- `dotnet ef database update`: apply EF Core migrations to SQLite.

Run commands from the repository root. The app loads `.env` credentials through DotNetEnv and expects `ADMIN_USERNAME` and `ADMIN_PASSWORD`.

## Coding Style & Naming Conventions

Use standard C# style: four-space indentation, PascalCase for classes, methods, and public properties, and camelCase for local variables and private fields. Nullable reference types and implicit usings are enabled; fix warnings instead of suppressing them.

Name controllers with the `Controller` suffix, services with the `Service` suffix, and Razor views after their MVC actions, such as `Views/PlaylistAdmin/Edit.cshtml`. Register shared services through dependency injection in `Program.cs`. Use descriptive asset names, for example `playlist-editor.js`.

## Testing Guidelines

No automated test project is currently present. Before opening a pull request, run `dotnet build` and manually verify public playlist browsing, admin login, playlist create/edit/delete flows, MP3 upload validation, and song reordering. If adding tests, create a separate project such as `MusicManager.Tests`, use xUnit or NUnit, and name test files after the unit under test.

## Commit & Pull Request Guidelines

Recent commits use short, direct summaries such as `First commit for ASP.NET version` and `Fix wrong Markdown format`. Follow that style with concise subject lines that describe the change.

Pull requests should include a brief summary, verification steps or test output, linked issues when applicable, and screenshots for visible UI changes. Call out any migration, configuration, or deployment impact.

## Security & Configuration Tips

Do not commit `.env`, uploaded MP3 files, database files, or secrets. Keep `wwwroot/mp3/.gitkeep` in place, but treat uploaded content as runtime data.


<claude-mem-context>
# Memory Context

# [music-playlist-manager-dev] recent context, 2026-06-07 4:27am UTC

No previous sessions found.
</claude-mem-context>