# Music Playlist Manager (Bun Runtime)

A full-featured web application for managing music playlists with an admin panel and public music player. Built with Express.js and optimized for Bun runtime.

## Features

- **Admin Panel**: Create, edit, and delete playlists with authentication
- **File Upload**: Upload multiple MP3 files with validation
- **Drag & Drop**: Reorder songs in playlists easily
- **Public Player**: Beautiful music player for listening to playlists
- **Theme Toggle**: Switch between light and dark themes
- **SQLite Database**: Lightweight and efficient data storage
- **Responsive Design**: Works on desktop and mobile devices
- **Bun Runtime**: Ultra-fast JavaScript runtime for better performance

## Prerequisites

- Bun (v1.0 or higher)
- Linux server (Ubuntu/Debian recommended)

## Installation Steps (Development)

### 1. Install Bun

```bash
curl -fsSL https://bun.sh/install | bash
```

After installation, restart your terminal or run:
```bash
source ~/.bashrc  # or source ~/.zshrc if using zsh
```

Verify installation:
```bash
bun --version
```

### 2. Create Project Directory

```bash
mkdir music-playlist-manager
cd music-playlist-manager
```

### 3. Create Directory Structure

```bash
mkdir -p src/{config,middleware,routes,views/{admin,public}}
mkdir -p public/{css,js}
mkdir -p mp3
```

### 4. Copy All Files

Copy all the files from the artifacts to their respective locations:
- `package.json` → root directory
- `.env` → root directory
- `README.md` → root directory
- Source files → `src/` directory
- View files → `src/views/` directory

### 5. Install Dependencies

```bash
bun install
```

### 6. Configure Environment Variables

Edit `.env` file and update the credentials:

```bash
nano .env
```

Change the following values:
- `ADMIN_USERNAME`: Your admin username
- `ADMIN_PASSWORD`: Your admin password
- `SESSION_SECRET`: A strong random secret key

### 7. Start the Application

For development (with auto-reload):
```bash
bun run dev
```

For production:
```bash
bun run start
```

### 8. Access the Application

- **Public Site**: http://localhost:3000
- **Admin Panel**: http://localhost:3000/admin/login

## Linux Server Setup (Production with Bun)

### Step 1: Update System

```bash
sudo apt update
sudo apt upgrade -y
```

### Step 2: Install Bun

```bash
# Install Bun
curl -fsSL https://bun.sh/install | bash

# Add Bun to PATH for all users
echo 'export BUN_INSTALL="$HOME/.bun"' | sudo tee -a /etc/profile
echo 'export PATH="$BUN_INSTALL/bin:$PATH"' | sudo tee -a /etc/profile

# For current session
source ~/.bashrc

# Verify installation
bun --version
```

### Step 3: Install Required System Dependencies

```bash
# Install unzip (required by Bun)
sudo apt install -y unzip wget curl

# Install build essentials (for better-sqlite3)
sudo apt install -y build-essential python3
```

### Step 4: Create Application User (Security Best Practice)

```bash
# Create user for running the app
sudo useradd -r -s /bin/bash -m -d /opt/music-app musicapp

# Switch to the user
sudo su - musicapp
```

### Step 5: Upload and Setup Project

```bash
# If using Git
cd /opt/music-app
git clone your-repository-url .

# OR if uploading files manually via SCP
# From your local machine:
# scp -r music-playlist-manager/* user@server:/opt/music-app/

# Set correct permissions
sudo chown -R musicapp:musicapp /opt/music-app
sudo chmod -R 755 /opt/music-app
```

### Step 6: Install Project Dependencies

```bash
cd /opt/music-app
bun install --production
```

### Step 7: Configure Environment Variables

```bash
nano .env
```

Update with production values:
```bash
PORT=3000
ADMIN_USERNAME=your_admin_username
ADMIN_PASSWORD=your_strong_password
SESSION_SECRET=your_very_strong_random_secret_key_change_this
NODE_ENV=production
```

### Step 8: Create Systemd Service

Create a systemd service file for automatic startup:

```bash
sudo nano /etc/systemd/system/music-app.service
```

Add the following content:

```ini
[Unit]
Description=Music Playlist Manager
After=network.target

[Service]
Type=simple
User=musicapp
WorkingDirectory=/opt/music-app
ExecStart=/home/musicapp/.bun/bin/bun run start
Restart=always
RestartSec=10
StandardOutput=syslog
StandardError=syslog
SyslogIdentifier=music-app
Environment=NODE_ENV=production

[Install]
WantedBy=multi-user.target
```

### Step 9: Start the Service

```bash
# Reload systemd
sudo systemctl daemon-reload

# Enable service to start on boot
sudo systemctl enable music-app

# Start the service
sudo systemctl start music-app

# Check status
sudo systemctl status music-app

# View logs
sudo journalctl -u music-app -f
```

### Step 10: Configure Nginx as Reverse Proxy

Install Nginx:
```bash
sudo apt install nginx -y
```

Create Nginx configuration:
```bash
sudo nano /etc/nginx/sites-available/music-app
```

Add this configuration:
```nginx
server {
    listen 80;
    server_name your-domain.com www.your-domain.com;

    # Increase body size for MP3 uploads
    client_max_body_size 50M;

    location / {
        proxy_pass http://localhost:3000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_cache_bypass $http_upgrade;
    }

    location /mp3 {
        alias /opt/music-app/mp3;
        autoindex off;
    }
}
```

Enable the site:
```bash
sudo ln -s /etc/nginx/sites-available/music-app /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl restart nginx
```

### Step 11: Configure Firewall

```bash
sudo ufw allow 'Nginx Full'
sudo ufw allow OpenSSH
sudo ufw enable
sudo ufw status
```

### Step 12: Setup SSL with Let's Encrypt (Optional but Recommended)

```bash
# Install Certbot
sudo apt install certbot python3-certbot-nginx -y

# Get SSL certificate
sudo certbot --nginx -d your-domain.com -d www.your-domain.com

# Auto-renewal is enabled by default. Test it:
sudo certbot renew --dry-run
```

## Systemd Service Management Commands

```bash
# Start service
sudo systemctl start music-app

# Stop service
sudo systemctl stop music-app

# Restart service
sudo systemctl restart music-app

# Check status
sudo systemctl status music-app

# View logs
sudo journalctl -u music-app -f

# View last 100 lines
sudo journalctl -u music-app -n 100

# Disable auto-start
sudo systemctl disable music-app

# Enable auto-start
sudo systemctl enable music-app
```

## Performance Optimization with Bun

Bun provides significant performance improvements over Node.js:

- **Faster startup time**: Up to 4x faster than Node.js
- **Lower memory usage**: More efficient memory management
- **Built-in bundler**: No need for webpack or other bundlers
- **Faster package installation**: `bun install` is much faster than `npm install`

### Monitoring Performance

```bash
# Check memory usage
ps aux | grep bun

# Monitor with htop
sudo apt install htop
htop
```

## Backup Strategy

### Backup Script

Create a backup script:
```bash
sudo nano /opt/music-app/backup.sh
```

Add:
```bash
#!/bin/bash
BACKUP_DIR="/opt/music-app-backups"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

mkdir -p $BACKUP_DIR

# Backup database
cp /opt/music-app/database.sqlite $BACKUP_DIR/database_$TIMESTAMP.sqlite

# Backup mp3 files
tar -czf $BACKUP_DIR/mp3_$TIMESTAMP.tar.gz -C /opt/music-app mp3/

# Keep only last 7 days of backups
find $BACKUP_DIR -name "database_*.sqlite" -mtime +7 -delete
find $BACKUP_DIR -name "mp3_*.tar.gz" -mtime +7 -delete

echo "Backup completed: $TIMESTAMP"
```

Make executable:
```bash
sudo chmod +x /opt/music-app/backup.sh
```

### Setup Cron Job for Daily Backups

```bash
sudo crontab -e
```

Add:
```bash
0 2 * * * /opt/music-app/backup.sh >> /var/log/music-app-backup.log 2>&1
```

## File Structure

```
music-playlist-manager/
├── src/
│   ├── config/
│   │   └── database.js          # SQLite configuration
│   ├── middleware/
│   │   └── auth.js              # Authentication middleware
│   ├── routes/
│   │   ├── admin.js             # Admin routes
│   │   └── public.js            # Public routes
│   ├── views/
│   │   ├── admin/               # Admin templates
│   │   └── public/              # Public templates
│   └── app.js                   # Main application
├── public/                      # Static assets
├── mp3/                         # Uploaded MP3 files
├── database.sqlite              # SQLite database
├── .env                         # Environment variables
└── package.json                 # Dependencies
```

## Troubleshooting

### Port Already in Use
```bash
# Find process using port 3000
sudo lsof -i :3000
# Kill the process
sudo kill -9 <PID>
# Or restart the service
sudo systemctl restart music-app
```

### Permission Issues with MP3 Directory
```bash
sudo chown -R musicapp:musicapp /opt/music-app/mp3
sudo chmod -R 755 /opt/music-app/mp3
```

### Database Locked Error
```bash
# Stop the service
sudo systemctl stop music-app
# Remove database lock files
cd /opt/music-app
rm -f database.sqlite-wal database.sqlite-shm
# Restart service
sudo systemctl start music-app
```

### Bun Not Found Error
```bash
# Make sure Bun is in PATH
echo $PATH
# If not, add to PATH
export PATH="$HOME/.bun/bin:$PATH"
# Or reinstall Bun
curl -fsSL https://bun.sh/install | bash
```

### Service Won't Start
```bash
# Check service status
sudo systemctl status music-app

# Check logs for errors
sudo journalctl -u music-app -n 50

# Test manually
cd /opt/music-app
bun run start
```

### High Memory Usage
```bash
# Monitor memory
free -h
# Check specific process
ps aux | grep bun
# Restart if needed
sudo systemctl restart music-app
```

## Updating the Application

```bash
# Stop service
sudo systemctl stop music-app

# Backup current version
sudo cp -r /opt/music-app /opt/music-app-backup-$(date +%Y%m%d)

# Update code (via git or upload)
cd /opt/music-app
git pull
# OR upload new files

# Install new dependencies
bun install --production

# Restart service
sudo systemctl start music-app

# Check status
sudo systemctl status music-app
```

## Security Checklist

- ✅ Use strong admin password
- ✅ Change SESSION_SECRET to random string
- ✅ Enable firewall (ufw)
- ✅ Use HTTPS (Let's Encrypt)
- ✅ Run app as non-root user
- ✅ Keep system updated
- ✅ Regular backups
- ✅ Monitor logs for suspicious activity
- ✅ Limit file upload size
- ✅ Use Nginx reverse proxy

## Performance Tips

1. **Use SSD storage** for database and MP3 files
2. **Enable Nginx caching** for static files
3. **Compress MP3 files** before upload if needed
4. **Monitor disk space** regularly
5. **Set up log rotation** to prevent disk fill-up

## License

ISC

## Support

For issues:
1. Check service status: `sudo systemctl status music-app`
2. View logs: `sudo journalctl -u music-app -f`
3. Test manually: `cd /opt/music-app && bun run start`