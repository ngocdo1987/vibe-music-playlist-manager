# Music Playlist Manager - Setup Guide

## Prerequisites

### 1. Install Required Dependencies on Linux

```bash
# Update package list
sudo apt update

# Install build tools
sudo apt install -y build-essential cmake git

# Install required libraries
sudo apt install -y libjsoncpp-dev uuid-dev zlib1g-dev openssl libssl-dev sqlite3 libsqlite3-dev

# Install Drogon dependencies
sudo apt install -y libbrotli-dev
```

### 2. Install Drogon Framework

```bash
# Clone Drogon repository
cd ~
git clone https://github.com/drogonframework/drogon
cd drogon
git submodule update --init

# Build and install
mkdir build
cd build
cmake ..
make -j$(nproc)
sudo make install

# Update library cache
sudo ldconfig
```

## Project Setup

### 1. Create Project Directory

```bash
# Create project directory
mkdir -p ~/music-playlist-manager
cd ~/music-playlist-manager
```

### 2. Create Directory Structure

```bash
# Create all necessary directories
mkdir -p controllers models filters views public/css public/js mp3 logs
```

### 3. Copy All Files

Copy all the files from the artifacts to their respective locations:

**Main files:**
- `CMakeLists.txt` → Root directory
- `main.cc` → Root directory
- `database.sql` → Root directory
- `config.json` → Root directory
- `.env` → Root directory

**Controllers:**
- `AuthController.h` → `controllers/`
- `AuthController.cc` → `controllers/`
- `PlaylistController.h` → `controllers/`
- `PlaylistController.cc` → `controllers/`

**Models:**
- `Song.h` → `models/`
- `Song.cc` → `models/`
- `Playlist.h` → `models/`
- `Playlist.cc` → `models/`

**Filters:**
- `AuthFilter.h` → `filters/`
- `AuthFilter.cc` → `filters/`

**Views:**
- `admin_login.csp` → `views/`
- `admin_dashboard.csp` → `views/`
- `playlist_form.csp` → `views/`
- `player.csp` → `views/`

**Public files:**
- `admin.css` → `public/css/`
- `admin.js` → `public/js/`

### 4. Configure Environment Variables

Edit the `.env` file:

```bash
nano .env
```

Change the default credentials:

```
ADMIN_USERNAME=your_username
ADMIN_PASSWORD=your_secure_password
```

### 5. Build the Project

```bash
# Create build directory
mkdir build
cd build

# Run CMake
cmake ..

# Build the project
make -j$(nproc)
```

## Running the Application

### 1. Run Directly

```bash
cd build
./MusicPlaylistManager
```

The application will be available at:
- Frontend: `http://your-server-ip:8080`
- Admin: `http://your-server-ip:8080/admin/login`

### 2. Run as a Systemd Service (Production)

Create a systemd service file:

```bash
sudo nano /etc/systemd/system/music-playlist-manager.service
```

Add the following content:

```ini
[Unit]
Description=Music Playlist Manager
After=network.target

[Service]
Type=simple
User=your_username
WorkingDirectory=/home/your_username/music-playlist-manager/build
ExecStart=/home/your_username/music-playlist-manager/build/MusicPlaylistManager
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Enable and start the service:

```bash
# Reload systemd
sudo systemctl daemon-reload

# Enable service to start on boot
sudo systemctl enable music-playlist-manager

# Start the service
sudo systemctl start music-playlist-manager

# Check status
sudo systemctl status music-playlist-manager
```

### 3. Setup Nginx Reverse Proxy (Optional)

Install Nginx:

```bash
sudo apt install -y nginx
```

Create Nginx configuration:

```bash
sudo nano /etc/nginx/sites-available/music-playlist-manager
```

Add configuration:

```nginx
server {
    listen 80;
    server_name your-domain.com;

    client_max_body_size 100M;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

Enable the site:

```bash
sudo ln -s /etc/nginx/sites-available/music-playlist-manager /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl restart nginx
```

## Usage Guide

### Admin Panel

1. **Login**
   - Navigate to `/admin/login`
   - Enter your credentials from `.env`

2. **Create Playlist**
   - Click "Create New Playlist"
   - Enter playlist name and description
   - Upload MP3 files (multiple files supported)
   - Drag and drop to reorder songs
   - Click "Save Playlist"

3. **Edit Playlist**
   - Click "Edit" on any playlist
   - Modify name, description
   - Add more songs or remove existing ones
   - Reorder by drag and drop
   - Click "Save Playlist"

4. **Delete Playlist**
   - Click "Delete" on any playlist
   - Confirm deletion

5. **Theme Toggle**
   - Click the moon/sun icon in the navbar to switch between light/dark mode

### Public Player

1. From the dashboard, click "View Player" on any playlist
2. The player will open in a new tab
3. Features:
   - Play/pause controls
   - Previous/next track buttons
   - Progress bar with seek functionality
   - Full playlist display
   - Click any song in the list to play it
   - Auto-play next song when current song ends

## Troubleshooting

### Database Issues

If you encounter database errors:

```bash
cd build
rm music_playlist.db
./MusicPlaylistManager
```

### Permission Issues

Ensure the application has write permissions:

```bash
chmod -R 755 ~/music-playlist-manager
chmod -R 777 ~/music-playlist-manager/build/mp3
chmod -R 777 ~/music-playlist-manager/build/logs
```

### Port Already in Use

If port 8080 is already in use, edit `config.json`:

```json
{
    "listeners": [
        {
            "address": "0.0.0.0",
            "port": 8081,  // Change to available port
            "https": false
        }
    ],
    ...
}
```

### View Logs

```bash
# If running with systemd
sudo journalctl -u music-playlist-manager -f

# If running directly, logs are in
tail -f logs/music-app.log
```

## Security Recommendations

1. **Change Default Credentials**
   - Always change the default admin credentials in `.env`

2. **Use HTTPS**
   - Setup SSL certificate with Let's Encrypt
   - Configure Nginx with SSL

3. **Firewall**
   ```bash
   sudo ufw allow 80/tcp
   sudo ufw allow 443/tcp
   sudo ufw enable
   ```

4. **File Upload Limits**
   - The default max file size is 50MB (configured in `config.json`)
   - Adjust based on your needs

5. **Backup Database Regularly**
   ```bash
   # Create backup script
   cp build/music_playlist.db backups/music_playlist_$(date +%Y%m%d_%H%M%S).db
   ```

## Updating the Application

```bash
# Stop the service
sudo systemctl stop music-playlist-manager

# Rebuild
cd ~/music-playlist-manager/build
make -j$(nproc)

# Restart the service
sudo systemctl start music-playlist-manager
```

## Support

For issues or questions:
- Check the logs first
- Verify all dependencies are installed
- Ensure database permissions are correct
- Check that port 8080 is not blocked by firewall