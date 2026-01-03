# Music Manager

A Common Lisp web application for managing MP3 playlists. Features a public frontend for music playback and a protected admin backend for playlist management.

## Features

- **Public Frontend**: Browse and play playlists without login
- **Admin Backend**: Secure login, playlist CRUD, MP3 upload
- **Drag & Drop**: Reorder songs easily with drag-and-drop
- **MP3 Validation**: Validates file extension and magic bytes
- **Theme Toggle**: Switch between light and dark modes
- **Responsive**: Works on desktop and mobile

## Tech Stack

- **Lisp**: SBCL (Steel Bank Common Lisp)
- **Web Server**: Hunchentoot with easy-routes
- **Templates**: Djula
- **Database**: SQLite via cl-dbi
- **Frontend**: Bootstrap 5, SortableJS

## Prerequisites

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- SQLite3

## Installation

### 1. Install SBCL

**Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install sbcl sqlite3 libsqlite3-dev
```

**macOS:**
```bash
brew install sbcl sqlite3
```

### 2. Install Quicklisp

```bash
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' --eval '(ql:add-to-init-file)' --quit
```

### 3. Clone and Link Project

```bash
# Clone to your projects directory
git clone <your-repo-url> ~/projects/music-manager
cd ~/projects/music-manager

# Link to Quicklisp local-projects
ln -s $(pwd) ~/.quicklisp/local-projects/music-manager
```

### 4. Configure Environment

```bash
cp .env.example .env
nano .env
```

Edit `.env` with your settings:
```
ADMIN_USER=admin
ADMIN_PASS=your-secure-password
SERVER_PORT=8080
SERVER_HOST=0.0.0.0
DB_PATH=music-manager.db
SESSION_SECRET=your-random-secret-string
```

### 5. Start the Application

```bash
sbcl --eval "(ql:quickload :music-manager)" \
     --eval "(music-manager:start-server)"
```

Or interactively:
```lisp
;; In SBCL REPL
(ql:quickload :music-manager)
(music-manager:start-server)
```

Access the application:
- **Frontend**: http://localhost:8080
- **Admin**: http://localhost:8080/admin

## Development

```lisp
;; Start in development mode
(ql:quickload :music-manager)
(music-manager:dev)

;; Restart after code changes
(music-manager:restart-server)

;; Stop server
(music-manager:stop-server)
```

## Production Deployment

### Using systemd

Create `/etc/systemd/system/music-manager.service`:

```ini
[Unit]
Description=Music Manager Lisp Web App
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/var/www/music-manager
ExecStart=/usr/bin/sbcl --load /var/www/music-manager/start.lisp
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
```

Create `/var/www/music-manager/start.lisp`:

```lisp
(ql:quickload :music-manager)
(music-manager:start-server)
(loop (sleep 86400))
```

Enable and start:
```bash
sudo systemctl enable music-manager
sudo systemctl start music-manager
```

### With Nginx Reverse Proxy

```nginx
server {
    listen 80;
    server_name music.example.com;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }

    location /mp3/ {
        alias /var/www/music-manager/mp3/;
        expires 7d;
        add_header Cache-Control "public, immutable";
    }

    location /static/ {
        alias /var/www/music-manager/static/;
        expires 7d;
    }
}
```

## Project Structure

```
music-manager/
├── music-manager.asd      # ASDF system definition
├── .env                   # Configuration (not in git)
├── .env.example           # Config template
├── src/
│   ├── package.lisp       # Package definition
│   ├── config.lisp        # Environment loading
│   ├── db.lisp            # Database connection/schema
│   ├── models.lisp        # Data access layer
│   ├── auth.lisp          # Authentication
│   ├── upload.lisp        # MP3 upload handling
│   ├── routes.lisp        # HTTP endpoints
│   └── main.lisp          # Server entry point
├── templates/
│   ├── base.html          # Base layout
│   ├── admin/             # Admin templates
│   └── public/            # Public templates
├── static/
│   ├── css/custom.css     # Custom styles
│   └── js/                # JavaScript files
└── mp3/                   # Uploaded MP3 files
```

## API Endpoints

| Method | Path | Auth | Description |
|--------|------|------|-------------|
| GET | `/` | No | Home (redirect to first playlist) |
| GET | `/playlist/:id` | No | Public playlist player |
| GET | `/admin/login` | No | Login form |
| POST | `/admin/login` | No | Process login |
| GET | `/admin/logout` | Yes | Logout |
| GET | `/admin` | Yes | Dashboard |
| GET | `/admin/playlist/new` | Yes | New playlist form |
| POST | `/admin/playlist` | Yes | Create playlist |
| GET | `/admin/playlist/:id/edit` | Yes | Edit playlist |
| POST | `/admin/playlist/:id` | Yes | Update playlist |
| POST | `/admin/playlist/:id/delete` | Yes | Delete playlist |
| POST | `/admin/upload` | Yes | Upload MP3 files (JSON) |
| POST | `/admin/playlist/:id/order` | Yes | Update song order (JSON) |

## License

MIT
