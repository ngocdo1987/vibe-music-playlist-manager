# Music Manager Web Application - Implementation Plan

A comprehensive ASP.NET Core 9 web application for managing MP3 files and playlists with SQLite database backend.

## User Review Required

> [!IMPORTANT]
> This application will use **ASP.NET Core 9**. Please confirm you have .NET 9 SDK installed on your Linux server.

> [!NOTE]
> The authentication system will use a simple username/password stored in `.env` file. This is suitable for single-admin scenarios but not recommended for production with multiple users.

## Proposed Changes

### Project Structure

The application will be organized as follows:

```
music-manager/
├── Program.cs                          # Application entry point
├── appsettings.json                   # App configuration
├── .env                               # Admin credentials (not committed to git)
├── .gitignore                         # Git ignore file
├── music-manager.csproj              # Project file
├── Data/
│   ├── MusicDbContext.cs             # EF Core DbContext
│   └── Migrations/                    # EF Core migrations
├── Models/
│   ├── Song.cs                       # Song entity
│   ├── Playlist.cs                   # Playlist entity
│   └── PlaylistSong.cs              # Junction table for many-to-many
├── Services/
│   ├── AuthService.cs                # Authentication service
│   └── FileService.cs                # File upload and validation
├── Controllers/
│   ├── AuthController.cs             # Login/logout
│   ├── AdminController.cs            # Admin dashboard
│   ├── PlaylistAdminController.cs    # Playlist CRUD (admin)
│   └── PublicController.cs           # Public frontend
├── Views/
│   ├── Shared/
│   │   ├── _Layout.cshtml           # Main layout
│   │   └── _AdminLayout.cshtml      # Admin layout with theme toggle
│   ├── Auth/
│   │   └── Login.cshtml             # Login page
│   ├── Admin/
│   │   └── Index.cshtml             # Admin dashboard
│   ├── PlaylistAdmin/
│   │   ├── Index.cshtml             # Playlist list
│   │   ├── Create.cshtml            # Create playlist
│   │   ├── Edit.cshtml              # Edit playlist (with drag-drop)
│   │   └── Delete.cshtml            # Delete confirmation
│   └── Public/
│       ├── Index.cshtml             # Public homepage
│       └── Player.cshtml            # Playlist player
├── wwwroot/
│   ├── css/
│   │   └── site.css                 # Custom styles
│   ├── js/
│   │   ├── theme-toggle.js          # Theme switcher
│   │   └── playlist-editor.js       # Drag & drop functionality
│   ├── lib/
│   │   └── bootstrap/               # Bootstrap files
│   └── mp3/                         # Uploaded MP3 files
└── Middleware/
    └── AdminAuthMiddleware.cs       # Admin authentication middleware
```

---

### Core Components

#### [NEW] [music-manager.csproj](file:///home/ngoc/projects/pet/csharp/music-manager/music-manager.csproj)

ASP.NET Core 9 web application with required NuGet packages:
- Microsoft.EntityFrameworkCore.Sqlite
- Microsoft.EntityFrameworkCore.Design
- Microsoft.AspNetCore.Session
- DotNetEnv (for .env file support)

---

#### [NEW] [Program.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Program.cs)

Application startup configuration:
- Configure SQLite database with EF Core
- Setup MVC with views
- Configure session management for authentication
- Setup static files serving
- Load environment variables from .env

---

### Database Layer

#### [NEW] [Models/Song.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Models/Song.cs)

Song entity with properties:
- Id (primary key)
- FileName (original filename)
- FilePath (storage path)
- Title (extracted from filename or user input)
- DurationSeconds (optional)
- CreatedAt

#### [NEW] [Models/Playlist.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Models/Playlist.cs)

Playlist entity with properties:
- Id (primary key)
- Name
- Description
- CreatedAt
- UpdatedAt
- Navigation property: PlaylistSongs

#### [NEW] [Models/PlaylistSong.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Models/PlaylistSong.cs)

Junction table for many-to-many relationship:
- Id (primary key)
- PlaylistId (foreign key)
- SongId (foreign key)
- Order (for sorting)
- Navigation properties: Playlist, Song

#### [NEW] [Data/MusicDbContext.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Data/MusicDbContext.cs)

EF Core DbContext with:
- DbSet for Songs, Playlists, PlaylistSongs
- OnModelCreating for relationship configuration

---

### Authentication System

#### [NEW] [Services/AuthService.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Services/AuthService.cs)

Service to:
- Load admin credentials from .env file
- Validate username/password
- No database storage for credentials

#### [NEW] [Middleware/AdminAuthMiddleware.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Middleware/AdminAuthMiddleware.cs)

Middleware to:
- Protect /admin/* routes
- Check session for authentication
- Redirect to login if not authenticated

#### [NEW] [Controllers/AuthController.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Controllers/AuthController.cs)

Handles:
- GET /auth/login - Display login form
- POST /auth/login - Process login
- POST /auth/logout - Clear session and logout

#### [NEW] [Views/Auth/Login.cshtml](file:///home/ngoc/projects/pet/csharp/music-manager/Views/Auth/Login.cshtml)

Bootstrap-styled login form with username and password fields

---

### Admin Backend

#### [NEW] [Controllers/AdminController.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Controllers/AdminController.cs)

Admin dashboard controller

#### [NEW] [Controllers/PlaylistAdminController.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Controllers/PlaylistAdminController.cs)

CRUD operations for playlists:
- Index - List all playlists
- Create - Create new playlist with MP3 uploads
- Edit - Update playlist, add/remove songs, reorder via drag-drop
- Delete - Delete playlist

#### [NEW] [Services/FileService.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Services/FileService.cs)

File upload service:
- Validate file extension (.mp3 only)
- Validate MIME type (audio/mpeg)
- Generate unique filenames to prevent conflicts
- Save files to wwwroot/mp3 directory
- Return file metadata

#### [NEW] [Views/PlaylistAdmin/Create.cshtml](file:///home/ngoc/projects/pet/csharp/music-manager/Views/PlaylistAdmin/Create.cshtml)

Form for creating playlists:
- Playlist name and description
- Multiple file upload for MP3s
- Client-side validation

#### [NEW] [Views/PlaylistAdmin/Edit.cshtml](file:///home/ngoc/projects/pet/csharp/music-manager/Views/PlaylistAdmin/Edit.cshtml)

Advanced playlist editor:
- Edit playlist details
- Upload additional MP3 files
- Drag-and-drop interface for reordering songs
- Remove songs from playlist
- Uses JavaScript for drag-drop functionality

#### [NEW] [wwwroot/js/playlist-editor.js](file:///home/ngoc/projects/pet/csharp/music-manager/wwwroot/js/playlist-editor.js)

JavaScript for drag-and-drop song ordering:
- HTML5 Drag and Drop API
- Update song order in UI
- Send updated order to backend via AJAX

---

### UI/UX - Theme System

#### [NEW] [Views/Shared/_AdminLayout.cshtml](file:///home/ngoc/projects/pet/csharp/music-manager/Views/Shared/_AdminLayout.cshtml)

Admin layout with:
- Bootstrap 5 integration
- Theme toggle button in navbar
- Light/dark theme support
- Navigation menu

#### [NEW] [wwwroot/js/theme-toggle.js](file:///home/ngoc/projects/pet/csharp/music-manager/wwwroot/js/theme-toggle.js)

Theme toggle functionality:
- Switch between light and dark themes
- Persist preference in localStorage
- Apply Bootstrap theme classes

#### [NEW] [wwwroot/css/site.css](file:///home/ngoc/projects/pet/csharp/music-manager/wwwroot/css/site.css)

Custom CSS for:
- Theme-specific color variables
- Drag-and-drop visual feedback
- Audio player styling
- Responsive design enhancements

---

### Public Frontend

#### [NEW] [Controllers/PublicController.cs](file:///home/ngoc/projects/pet/csharp/music-manager/Controllers/PublicController.cs)

Public-facing controllers:
- Index - List all playlists
- Player/{id} - Play specific playlist

#### [NEW] [Views/Public/Index.cshtml](file:///home/ngoc/projects/pet/csharp/music-manager/Views/Public/Index.cshtml)

Public homepage:
- List all available playlists
- Links to player pages
- Clean, simple design

#### [NEW] [Views/Public/Player.cshtml](file:///home/ngoc/projects/pet/csharp/music-manager/Views/Public/Player.cshtml)

Playlist player:
- HTML5 audio player
- Display playlist name and song list
- Auto-advance to next song
- Show current playing song
- Responsive design

---

### Configuration Files

#### [NEW] [.env](file:///home/ngoc/projects/pet/csharp/music-manager/.env)

Environment variables:
```
ADMIN_USERNAME=admin
ADMIN_PASSWORD=your_secure_password_here
```

#### [NEW] [.gitignore](file:///home/ngoc/projects/pet/csharp/music-manager/.gitignore)

Standard .NET gitignore plus:
- .env
- wwwroot/mp3/*
- *.db (SQLite database)

#### [NEW] [appsettings.json](file:///home/ngoc/projects/pet/csharp/music-manager/appsettings.json)

Application settings:
- SQLite connection string
- Logging configuration
- File upload limits

---

## Verification Plan

### Automated Tests

No automated tests will be created in this initial implementation. Testing will be manual.

### Manual Verification

1. **Project Setup**
   - Verify project builds successfully with `dotnet build`
   - Verify migrations can be applied with `dotnet ef database update`

2. **Authentication**
   - Test login with correct credentials
   - Test login with incorrect credentials
   - Verify admin routes are protected
   - Verify logout functionality

3. **Playlist Management**
   - Create new playlist
   - Upload multiple MP3 files
   - Verify only .mp3 files are accepted
   - Test drag-and-drop reordering
   - Edit playlist details
   - Delete playlist

4. **Theme Toggle**
   - Switch between light and dark themes in admin panel
   - Verify preference persists across page reloads

5. **Public Player**
   - Access playlist player as public user (no login)
   - Verify audio playback
   - Test auto-advance to next song
   - Test on different browsers

6. **File Storage**
   - Verify MP3 files are saved to `wwwroot/mp3/`
   - Verify files have unique names
   - Verify files are served correctly

### Documentation Verification

Create comprehensive setup guide including:
- Prerequisites (Linux, .NET 9 SDK)
- Installation steps
- Database setup
- Running the application
- Deployment considerations
