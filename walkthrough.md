# Music Manager Web Application - Walkthrough

## Project Overview

The Music Manager is a full-featured ASP.NET Core 9 web application for managing MP3 files and playlists. The application consists of:

- **Public Frontend**: Browse and play playlists without authentication
- **Admin Backend**: Secure management interface with authentication
- **SQLite Database**: Lightweight database for storing playlists and song metadata
- **File Management**: MP3 upload, validation, and storage

## Technology Stack

- **Framework**: ASP.NET Core 9 (MVC with Razor Pages)
- **Database**: SQLite with Entity Framework Core 9
- **Frontend**: Bootstrap 5, HTML5 Audio API, Vanilla JavaScript
- **Authentication**: Session-based with credentials from .env file

## Project Structure

```
music-manager/
├── Controllers/              # MVC Controllers
│   ├── AuthController.cs    # Login/Logout
│   ├── AdminController.cs   # Admin dashboard
│   ├── PlaylistAdminController.cs  # Playlist CRUD
│   └── PublicController.cs  # Public pages
├── Data/
│   └── MusicDbContext.cs    # EF Core DbContext
├── Middleware/
│   └── AdminAuthMiddleware.cs  # Authentication protection
├── Models/
│   ├── Song.cs              # Song entity
│   ├── Playlist.cs          # Playlist entity
│   └── PlaylistSong.cs      # Junction table
├── Services/
│   ├── AuthService.cs       # Authentication logic
│   └── FileService.cs       # File upload & validation
├── Views/                   # Razor views
└── wwwroot/                 # Static files & uploaded MP3s
```

## Setup Instructions

### Step 1: Prerequisites

Ensure you have .NET 9 SDK installed on your Linux server:

```bash
# Check if .NET is installed
dotnet --version

# If not installed, install .NET 9 SDK:
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
sudo apt-get update
sudo apt-get install -y dotnet-sdk-9.0
```

### Step 2: Configure Environment Variables

Create a `.env` file from the example:

```bash
cd /home/ngoc/projects/pet/csharp/music-manager
cp .env.example .env
nano .env
```

Update with your admin credentials:

```env
ADMIN_USERNAME=admin
ADMIN_PASSWORD=your_secure_password_here
```

> [!IMPORTANT]
> **Security Note**: Change the default password to something secure. The .env file is excluded from git for security.

### Step 3: Restore Dependencies

```bash
dotnet restore
```

This will download all required NuGet packages:
- Microsoft.EntityFrameworkCore.Sqlite
- Microsoft.EntityFrameworkCore.Design
- DotNetEnv

### Step 4: Create Database

Install EF Core tools globally (if not already installed):

```bash
dotnet tool install --global dotnet-ef
```

Create the initial migration:

```bash
dotnet ef migrations add InitialCreate
```

Apply the migration to create the SQLite database:

```bash
dotnet ef database update
```

This creates a `music.db` file in the project root with the following tables:
- `Songs` - MP3 file metadata
- `Playlists` - Playlist information
- `PlaylistSongs` - Many-to-many relationship with ordering

### Step 5: Create Upload Directory

Ensure the MP3 upload directory exists:

```bash
mkdir -p wwwroot/mp3
chmod 755 wwwroot/mp3
```

## Running the Application

### Development Mode

Start the application in development mode:

```bash
dotnet run
```

The application will be accessible at:
- **HTTP**: http://localhost:5000
- **HTTPS**: https://localhost:5001

### Production Build

For production deployment, build the application:

```bash
dotnet build --configuration Release
```

Run the production build:

```bash
dotnet bin/Release/net9.0/music-manager.dll
```

### Running as a systemd Service

For continuous operation, create a systemd service:

1. Create service file:
```bash
sudo nano /etc/systemd/system/music-manager.service
```

2. Add configuration:
```ini
[Unit]
Description=Music Manager Web Application
After=network.target

[Service]
WorkingDirectory=/home/ngoc/projects/pet/csharp/music-manager
ExecStart=/usr/bin/dotnet /home/ngoc/projects/pet/csharp/music-manager/bin/Release/net9.0/music-manager.dll
Restart=always
RestartSec=10
User=www-data
Environment=ASPNETCORE_ENVIRONMENT=Production

[Install]
WantedBy=multi-user.target
```

3. Enable and start:
```bash
sudo systemctl enable music-manager.service
sudo systemctl start music-manager.service
sudo systemctl status music-manager.service
```

## Features and Usage

### 1. Public Frontend (No Authentication Required)

#### Homepage - Browse Playlists
- **URL**: `/` or `/public/index`
- **Features**: 
  - View all available playlists in card layout
  - See playlist name, description, song count, and last updated date
  - Click "Play Playlist" to access the player

#### Playlist Player
- **URL**: `/public/player/{id}`
- **Features**:
  - HTML5 audio player with standard controls
  - Track list showing all songs in order
  - Click any track to jump to that song
  - Auto-advance to next track when current track ends
  - Visual indication of currently playing track

### 2. Admin Backend (Authentication Required)

#### Login Page
- **URL**: `/auth/login`
- **Credentials**: Username and password from .env file
- **Features**:
  - Secure session-based authentication
  - Gradient background design
  - Redirect to admin dashboard after successful login

#### Admin Dashboard
- **URL**: `/admin`
- **Features**:
  - Welcome message with username
  - Quick links to playlist management
  - Link to view public frontend

#### Playlist Management
- **URL**: `/playlistadmin`
- **Features**:
  - View all playlists in table format
  - See song count and last updated timestamp
  - Quick actions: Edit, Play, Delete

### 3. Playlist CRUD Operations

#### Create Playlist
- **URL**: `/playlistadmin/create`
- **Steps**:
  1. Enter playlist name (required)
  2. Enter description (optional)
  3. Upload one or more MP3 files
  4. Click "Create Playlist"

**Validation**:
- Only `.mp3` files accepted
- MIME type must be `audio/mpeg`
- File size limit: 50MB (configurable in appsettings.json)

#### Edit Playlist
- **URL**: `/playlistadmin/edit/{id}`
- **Features**:
  - Update playlist name and description
  - **Drag-and-drop reordering**: Click and drag songs to reorder
  - **Add songs**: Upload additional MP3 files
  - **Remove songs**: Click "Remove" button on any song
  - Changes to order are saved via AJAX automatically

**Drag-and-Drop Usage**:
1. Click the drag handle (☰) on any song
2. Drag to desired position
3. Drop to reorder
4. Order is saved automatically to the server

#### Delete Playlist
- **URL**: `/playlistadmin/delete/{id}`
- **Features**:
  - Confirmation page showing playlist details
  - Shows all songs that will be affected
  - Note: MP3 files remain on server (may be in other playlists)

### 4. Theme Toggle (Admin Panel Only)

- **Location**: Navbar in admin panel
- **Button**: "🌙 Toggle Theme" (icon changes based on theme)
- **Persistence**: Preference saved in browser localStorage
- **Themes**: 
  - Light mode (default)
  - Dark mode (dark backgrounds, adjusted colors)

## Database Schema

### Songs Table
| Column | Type | Description |
|--------|------|-------------|
| Id | INTEGER | Primary key |
| FileName | TEXT | Original filename |
| FilePath | TEXT | Server path (e.g., /mp3/guid.mp3) |
| Title | TEXT | Display title |
| DurationSeconds | INTEGER | Optional duration |
| CreatedAt | DATETIME | Upload timestamp |

### Playlists Table
| Column | Type | Description |
|--------|------|-------------|
| Id | INTEGER | Primary key |
| Name | TEXT | Playlist name |
| Description | TEXT | Optional description |
| CreatedAt | DATETIME | Creation timestamp |
| UpdatedAt | DATETIME | Last modification |

### PlaylistSongs Table (Junction)
| Column | Type | Description |
|--------|------|-------------|
| Id | INTEGER | Primary key |
| PlaylistId | INTEGER | Foreign key to Playlists |
| SongId | INTEGER | Foreign key to Songs |
| Order | INTEGER | Position in playlist (0-indexed) |

## File Upload Flow

1. User selects MP3 file(s) in browser
2. Files are sent to server via multipart/form-data
3. **FileService** validates each file:
   - Check extension is `.mp3`
   - Check MIME type is `audio/mpeg`
   - Check file size within limits
4. If valid, file is saved to `wwwroot/mp3/` with unique GUID filename
5. **Song** entity is created in database with metadata
6. **PlaylistSong** junction record is created with order number

## Security Features

✅ **Session-based Authentication**: Admin routes protected by middleware  
✅ **Environment Variables**: Credentials stored in .env (not in code)  
✅ **File Validation**: Extension and MIME type checking  
✅ **HTTP-only Cookies**: Session cookies not accessible via JavaScript  
✅ **CSRF Protection**: ASP.NET Core built-in anti-forgery tokens  

## Troubleshooting

### Database Errors

**Problem**: "No such table" errors

**Solution**: Run migrations
```bash
dotnet ef database update
```

### Permission Denied on MP3 Upload

**Problem**: Cannot save uploaded files

**Solution**: Check directory permissions
```bash
chmod 755 wwwroot/mp3
```

### Port Already in Use

**Problem**: Address already in use

**Solution**: Change port or kill existing process
```bash
# Use different port
dotnet run --urls "http://localhost:5002"

# Or find and kill process on port 5000
sudo lsof -i :5000
sudo kill -9 <PID>
```

### Theme Not Persisting

**Problem**: Theme resets after page reload

**Solution**: Check browser localStorage is enabled and not cleared

### Songs Not Playing

**Problem**: Audio player shows errors

**Solution**:
1. Check MP3 file is valid
2. Verify file path in database matches actual file location
3. Check browser console for errors
4. Ensure wwwroot/mp3 directory is accessible

## Configuration Options

### appsettings.json

```json
{
  "FileUpload": {
    "MaxFileSizeMB": 50,          // Change max upload size
    "AllowedExtensions": [".mp3"], // Add more formats if needed
    "UploadPath": "wwwroot/mp3"   // Change upload directory
  }
}
```

### Environment Variables (.env)

```env
ADMIN_USERNAME=admin              # Change admin username
ADMIN_PASSWORD=secure_password    # Change admin password
```

## Deployment Considerations

### For Production Deployment

1. **Use HTTPS**: Configure SSL certificate
2. **Secure .env**: Ensure proper file permissions (chmod 600)
3. **Backup Database**: Regular backups of music.db
4. **Backup MP3 Files**: Regular backups of wwwroot/mp3/
5. **Rate Limiting**: Consider adding rate limiting middleware
6. **Reverse Proxy**: Use Nginx/Apache as reverse proxy
7. **Monitoring**: Setup logging and monitoring

### Nginx Reverse Proxy Example

```nginx
server {
    listen 80;
    server_name musicmanager.example.com;
    
    location / {
        proxy_pass http://localhost:5000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection keep-alive;
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

## Next Steps

After completing the setup, you can:

1. **Test the application** by creating your first playlist
2. **Customize the design** by editing CSS in wwwroot/css/site.css
3. **Add more features** like:
   - User registration and multiple admin accounts
   - Song metadata extraction (title, artist, album from ID3 tags)
   - Playlist sharing and embedding
   - Search and filtering
   - Public playlist creation

## Summary

The Music Manager web application provides a complete solution for managing and playing MP3 playlists with:

✅ Secure admin authentication  
✅ Full CRUD operations for playlists  
✅ Drag-and-drop song ordering  
✅ MP3 file validation and upload  
✅ Public-facing music player  
✅ Light/Dark theme support  
✅ Responsive Bootstrap design  
✅ SQLite database with EF Core  

All code is written in English with comprehensive comments, and the application is ready for deployment on a Linux server.
