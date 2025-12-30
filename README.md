# Music Manager

A pure Zig web application for managing MP3 playlists with a public frontend and password-protected admin backend.

## Features

- 🎵 **Public Player** - Browse playlists and play songs with HTML5 audio
- 🔒 **Admin Dashboard** - Secure login to manage playlists
- 📁 **MP3 Upload** - Drag-and-drop file uploads with validation
- 🔀 **Song Ordering** - Drag-and-drop to reorder songs in playlists
- 🌗 **Theme Toggle** - Dark/light mode with localStorage persistence
- 📦 **Pure Zig** - No external frameworks, just Zig standard library + SQLite

## Requirements

- Ubuntu 22.04+ (or any Linux with glibc)
- Zig 0.15.x
- SQLite3 development libraries

## Installation

### 1. Install Dependencies

```bash
# Update packages
sudo apt update

# Install SQLite development libraries
sudo apt install sqlite3 libsqlite3-dev

# Install Zig (if not already installed)
wget https://ziglang.org/builds/zig-linux-x86_64-0.15.0-dev.tar.xz
tar -xf zig-linux-x86_64-0.15.0-dev.tar.xz
sudo mv zig-linux-x86_64-0.15.0-dev /opt/zig
sudo ln -s /opt/zig/zig /usr/local/bin/zig

# Or use snap
sudo snap install zig --classic --beta
```

### 2. Clone and Build

```bash
# Clone the repository
git clone https://github.com/your-repo/music-manager.git
cd music-manager

# Build the application
zig build

# The binary is at: zig-out/bin/music-manager
```

### 3. Configure

Create a `.env` file in the project root:

```bash
# Admin credentials
ADMIN_USERNAME=admin
ADMIN_PASSWORD=your_secure_password

# Server port (optional, default: 8080)
SERVER_PORT=8080
```

### 4. Run

```bash
# Development mode
zig build run

# Or run the binary directly
./zig-out/bin/music-manager

# Server starts at http://localhost:8080
```

## Directory Structure

```
music-manager/
├── src/
│   ├── main.zig           # Entry point, HTTP server, handlers
│   ├── db.zig             # SQLite database layer
│   ├── router.zig         # HTTP request/response handling
│   ├── auth.zig           # Authentication & session management
│   ├── templates/         # HTML template generators
│   │   ├── layout.zig
│   │   ├── admin.zig
│   │   └── public.zig
│   └── utils/             # Utilities
│       ├── session.zig    # Session management
│       ├── multipart.zig  # File upload parsing
│       └── mp3.zig        # MP3 validation
├── public/
│   └── mp3/               # Uploaded MP3 files
├── .env                   # Environment configuration
├── build.zig              # Build configuration
└── music_manager.db       # SQLite database (auto-created)
```

## Docker Deployment

### Quick Start with Docker Compose

```bash
# Clone the repository
git clone https://github.com/your-repo/music-manager.git
cd music-manager

# Start the container
docker-compose up -d

# View logs
docker-compose logs -f

# Stop
docker-compose down
```

The app will be available at `http://localhost:8080`

### Configuration

Edit `docker-compose.yml` to change:

```yaml
environment:
  - ADMIN_USERNAME=admin
  - ADMIN_PASSWORD=your_secure_password
```

### Data Persistence

Volumes are configured for:
- `./data` - SQLite database
- `./public/mp3` - Uploaded MP3 files

### Build Only

```bash
# Build the Docker image
docker build -t music-manager .

# Run the container
docker run -d \
  --name music-manager \
  -p 8080:8080 \
  -e ADMIN_USERNAME=admin \
  -e ADMIN_PASSWORD=your_password \
  -v $(pwd)/data:/app/data \
  -v $(pwd)/public/mp3:/app/public/mp3 \
  music-manager
```

## Production Deployment

### Using systemd

Create `/etc/systemd/system/music-manager.service`:

```ini
[Unit]
Description=Music Manager Web Application
After=network.target

[Service]
Type=simple
User=www-data
Group=www-data
WorkingDirectory=/opt/music-manager
ExecStart=/opt/music-manager/zig-out/bin/music-manager
Restart=always
RestartSec=5
Environment=HOME=/opt/music-manager

[Install]
WantedBy=multi-user.target
```

Enable and start:

```bash
# Copy files to /opt
sudo mkdir -p /opt/music-manager
sudo cp -r . /opt/music-manager/
sudo chown -R www-data:www-data /opt/music-manager

# Enable service
sudo systemctl enable music-manager
sudo systemctl start music-manager
sudo systemctl status music-manager
```

### Nginx Reverse Proxy

Install and configure Nginx:

```bash
sudo apt install nginx
```

Create `/etc/nginx/sites-available/music-manager`:

```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    # Increase max upload size for MP3 files
    client_max_body_size 50M;
}
```

Enable site:

```bash
sudo ln -s /etc/nginx/sites-available/music-manager /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

### HTTPS with Certbot

```bash
sudo apt install certbot python3-certbot-nginx
sudo certbot --nginx -d your-domain.com
```

## Usage

### Admin Panel

1. Navigate to `http://localhost:8080/admin`
2. Login with credentials from `.env` file
3. Create playlists and upload MP3 files
4. Drag-and-drop to reorder songs

### Public Player

1. Visit `http://localhost:8080`
2. Click on a playlist to open the player
3. Songs auto-advance when finished

## Testing

```bash
# Run all tests
zig test src/db.zig
zig test src/router.zig
zig test src/utils/mp3.zig
zig test src/utils/multipart.zig
```

## License

MIT License
