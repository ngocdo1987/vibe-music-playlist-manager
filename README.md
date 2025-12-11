# Music Manager

A Rust web application for managing MP3 playlists built with Actix Web and SQLite.

## Features

- **Backend Admin Panel**: Secure login to manage playlists and songs
- **Playlist Management**: Create, edit, delete playlists with drag-and-drop song ordering
- **Song Upload**: Upload MP3 files with validation
- **Public Frontend**: Listen to playlists with a built-in HTML5 audio player
- **Light/Dark Theme**: Toggle between themes
- **Responsive Design**: Works on desktop and mobile

## Requirements

- Rust 1.70 or later
- Linux server (tested on Ubuntu 22.04)

## Installation

### Step 1: Install Rust

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
```

### Step 2: Clone or Create Project

```bash
mkdir music-manager
cd music-manager
```

Copy all the source files into the project directory with the following structure:

```
music-manager/
├── Cargo.toml
├── .env
├── src/
│   ├── main.rs
│   ├── models.rs
│   ├── db.rs
│   └── handlers/
│       ├── mod.rs
│       ├── auth.rs
│       ├── admin.rs
│       ├── api.rs
│       └── public.rs
├── templates/
│   ├── base.html
│   ├── auth/
│   │   └── login.html
│   ├── admin/
│   │   ├── base.html
│   │   ├── dashboard.html
│   │   ├── playlists.html
│   │   ├── playlist_form.html
│   │   └── songs.html
│   └── public/
│       ├── index.html
│       └── playlist.html
└── mp3/
    └── (uploaded files will be stored here)
```

### Step 3: Configure Environment

Copy `.env.example` to `.env` and update the values:

```bash
cp .env.example .env
nano .env
```

Set your admin credentials and a secure session secret:

```env
ADMIN_USERNAME=admin
ADMIN_PASSWORD=your_secure_password_here
HOST=0.0.0.0
PORT=8080
SESSION_SECRET=generate_a_random_64_character_string_here
```

To generate a random session secret:

```bash
openssl rand -hex 32
```

### Step 4: Build and Run

```bash
# Development mode
cargo run

# Production build
cargo build --release
./target/release/music-manager
```

### Step 5: Access the Application

- Public frontend: `http://your-server:8080/`
- Admin login: `http://your-server:8080/admin/login`

## Running as a Service (systemd)

Create a systemd service file:

```bash
sudo nano /etc/systemd/system/music-manager.service
```

Add the following content:

```ini
[Unit]
Description=Music Manager Web Application
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/path/to/music-manager
ExecStart=/path/to/music-manager/target/release/music-manager
Restart=on-failure
RestartSec=5
Environment=RUST_LOG=info

[Install]
WantedBy=multi-user.target
```

Enable and start the service:

```bash
sudo systemctl daemon-reload
sudo systemctl enable music-manager
sudo systemctl start music-manager
sudo systemctl status music-manager
```

## Nginx Reverse Proxy (Optional)

If you want to use Nginx as a reverse proxy:

```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        
        # For large file uploads
        client_max_body_size 100M;
    }
}
```

## Usage

### Admin Panel

1. Go to `/admin/login` and enter your credentials
2. Create a new playlist from the dashboard
3. Upload MP3 files from the playlist edit page
4. Drag and drop to reorder songs
5. Save the order

### Public Site

1. Visit the homepage to see all playlists
2. Click on a playlist to open the player
3. Use the audio controls or click songs in the list to play

## License

MIT License
