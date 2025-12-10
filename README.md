# Music Playlist Manager

A full-featured web application for managing music playlists with an admin panel and public music player.

## Features

- **Admin Panel**: Create, edit, and delete playlists with authentication
- **File Upload**: Upload multiple MP3 files with validation
- **Drag & Drop**: Reorder songs in playlists easily
- **Public Player**: Beautiful music player for listening to playlists
- **Theme Toggle**: Switch between light and dark themes
- **SQLite Database**: Lightweight and efficient data storage
- **Responsive Design**: Works on desktop and mobile devices

## Prerequisites

- Node.js (v14 or higher)
- npm or yarn
- Linux server (Ubuntu/Debian recommended)

## Installation Steps

### 1. Create Project Directory

```bash
mkdir music-playlist-manager
cd music-playlist-manager
```

### 2. Create Directory Structure

```bash
mkdir -p src/{config,middleware,routes,views/{admin,public}}
mkdir -p public/{css,js}
mkdir -p mp3
```

### 3. Copy All Files

Copy all the files from the artifacts to their respective locations:
- `package.json` → root directory
- `.env` → root directory
- `README.md` → root directory
- Source files → `src/` directory
- View files → `src/views/` directory

### 4. Install Dependencies

```bash
npm install
```

### 5. Configure Environment Variables

Edit `.env` file and update the credentials:

```bash
nano .env
```

Change the following values:
- `ADMIN_USERNAME`: Your admin username
- `ADMIN_PASSWORD`: Your admin password
- `SESSION_SECRET`: A strong random secret key

### 6. Start the Application

For development:
```bash
npm run dev
```

For production:
```bash
npm start
```

### 7. Access the Application

- **Public Site**: http://your-server-ip:3000
- **Admin Panel**: http://your-server-ip:3000/admin/login

## Linux Server Setup (Production)

### 1. Update System

```bash
sudo apt update
sudo apt upgrade -y
```

### 2. Install Node.js

```bash
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt install -y nodejs
```

### 3. Install PM2 (Process Manager)

```bash
sudo npm install -g pm2
```

### 4. Clone/Upload Project

Upload your project to the server (via FTP, Git, or SCP).

### 5. Install Dependencies

```bash
cd /path/to/music-playlist-manager
npm install --production
```

### 6. Start with PM2

```bash
pm2 start src/app.js --name music-app
pm2 save
pm2 startup
```

### 7. Configure Nginx (Optional)

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
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:3000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }

    location /mp3 {
        alias /path/to/music-playlist-manager/mp3;
    }
}
```

Enable the site:
```bash
sudo ln -s /etc/nginx/sites-available/music-app /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl restart nginx
```

### 8. Configure Firewall

```bash
sudo ufw allow 80
sudo ufw allow 443
sudo ufw enable
```

## Usage

### Admin Panel

1. Login at `/admin/login`
2. Create a new playlist
3. Upload MP3 files
4. Drag and drop to reorder songs
5. Save changes

### Public Player

1. Visit the homepage to see all playlists
2. Click on a playlist to play
3. Use playback controls or keyboard shortcuts:
   - Space: Play/Pause
   - Arrow Left: Previous song
   - Arrow Right: Next song

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

## Security Notes

- Change default admin credentials in `.env`
- Use strong passwords
- Keep `SESSION_SECRET` secure
- Consider using HTTPS in production
- Regularly backup the database and mp3 files

## Troubleshooting

### Port Already in Use
```bash
# Find process using port 3000
sudo lsof -i :3000
# Kill the process
sudo kill -9 <PID>
```

### Permission Issues with MP3 Directory
```bash
sudo chown -R $USER:$USER mp3/
sudo chmod -R 755 mp3/
```

### Database Locked Error
```bash
# Stop the application
pm2 stop music-app
# Remove database lock
rm database.sqlite-wal database.sqlite-shm
# Restart
pm2 start music-app
```

## License

ISC

## Support

For issues or questions, please check the application logs:
```bash
pm2 logs music-app
```