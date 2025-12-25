# Music Manager - ASP.NET Core 9 Web Application

A comprehensive web application for managing MP3 files and playlists, built with ASP.NET Core 9 and SQLite.

## Features

- **Frontend (Public)**
  - Browse playlists without authentication
  - Play playlists with HTML5 audio player
  - Auto-advance to next song
  - Responsive design

- **Backend (Admin)**
  - Secure login with username/password from .env file
  - Create, edit, and delete playlists
  - Upload multiple MP3 files
  - Drag-and-drop song reordering
  - MP3 file validation
  - Light/Dark theme toggle

## Prerequisites

- .NET 9 SDK
- SQLite (usually included with .NET)
- Linux server (Ubuntu, Debian, etc.)

## Installation on Linux

### 1. Install .NET 9 SDK

```bash
# Download Microsoft package repository
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

# Install .NET SDK
sudo apt-get update
sudo apt-get install -y dotnet-sdk-9.0
```

### 2. Clone or Copy Project Files

Navigate to the project directory:
```bash
cd /home/ngoc/projects/pet/csharp/music-manager
```

### 3. Setup Environment Variables

Copy the example .env file and edit credentials:
```bash
cp .env.example .env
nano .env
```

Update with your desired admin credentials:
```
ADMIN_USERNAME=your_admin_username
ADMIN_PASSWORD=your_secure_password
```

### 4. Restore Dependencies

```bash
dotnet restore
```

### 5. Create Database

Install EF Core tools globally (if not already installed):
```bash
dotnet tool install --global dotnet-ef
```
or
```bash
dotnet tool install --global dotnet-ef --version 9.*
```

Create initial migration and database:
```bash
dotnet ef migrations add InitialCreate
dotnet ef database update
```

### 6. Create MP3 Upload Directory

```bash
mkdir -p wwwroot/mp3
```

## Running the Application

### Development Mode

```bash
dotnet run
```

The application will be available at:
- HTTP: http://localhost:5000
- HTTPS: https://localhost:5001

### Production Mode

Build the application:
```bash
dotnet build --configuration Release
```

Run the built application:
```bash
dotnet bin/Release/net9.0/music-manager.dll
```

### Running as a Service (systemd)

Create a service file:
```bash
sudo nano /etc/systemd/system/music-manager.service
```

Add the following content (adjust paths as needed):
```ini
[Unit]
Description=Music Manager Web Application
After=network.target

[Service]
WorkingDirectory=/home/ngoc/projects/pet/csharp/music-manager
ExecStart=/usr/bin/dotnet /home/ngoc/projects/pet/csharp/music-manager/bin/Release/net9.0/music-manager.dll
Restart=always
RestartSec=10
KillSignal=SIGINT
SyslogIdentifier=music-manager
User=www-data
Environment=ASPNETCORE_ENVIRONMENT=Production
Environment=DOTNET_PRINT_TELEMETRY_MESSAGE=false

[Install]
WantedBy=multi-user.target
```

Enable and start the service:
```bash
sudo systemctl enable music-manager.service
sudo systemctl start music-manager.service
sudo systemctl status music-manager.service
```

## Project Structure

```
music-manager/
├── Controllers/          # MVC controllers
├── Data/                # Database context and migrations
├── Middleware/          # Custom middleware (authentication)
├── Models/              # Entity models
├── Services/            # Business logic services
├── Views/               # Razor views
├── wwwroot/             # Static files
│   ├── css/            # Stylesheets
│   ├── js/             # JavaScript files
│   └── mp3/            # Uploaded MP3 files
├── Program.cs           # Application entry point
├── appsettings.json    # Configuration
└── .env                # Environment variables (not in git)
```

## Usage

### Admin Panel

1. Navigate to `/auth/login`
2. Enter admin credentials from .env file
3. Access admin dashboard at `/admin`
4. Manage playlists at `/playlistadmin`

### Creating a Playlist

1. Click "Create New Playlist" in admin panel
2. Enter playlist name and description
3. Upload one or more MP3 files
4. Click "Create Playlist"

### Editing a Playlist

1. Click "Edit" on a playlist
2. Update name/description
3. Drag and drop songs to reorder
4. Upload additional MP3 files
5. Remove songs as needed
6. Click "Update Playlist"

### Public Access

1. Navigate to `/` to see all playlists
2. Click on a playlist to open the player
3. Songs will play in order and auto-advance

## Troubleshooting

### Database Issues

If you encounter database errors, try recreating the database:
```bash
rm music.db
dotnet ef database update
```

### Permission Issues

Ensure the application has write permissions for:
- Database file location
- wwwroot/mp3 directory

```bash
chmod 755 wwwroot/mp3
```

### Port Already in Use

Change the port in `Properties/launchSettings.json` or use:
```bash
dotnet run --urls "http://localhost:5002"
```

## Security Considerations

- Change default admin credentials in .env file
- Use HTTPS in production
- Keep .env file secure (never commit to git)
- Consider implementing rate limiting for production
- Regular backup of music.db and mp3 files

## License

This project is for educational purposes.
