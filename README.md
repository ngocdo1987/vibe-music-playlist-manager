# Fortran Music Manager

A web application for managing MP3 songs and playlists, built entirely in Fortran using the FastCGI protocol with Nginx and SQLite.

## Features

- **Public Frontend**: Browse playlists and play music without login
- **Admin Backend**: Secure login to manage playlists and songs
- **Playlist Management**: Create, edit, and delete playlists
- **Drag & Drop Ordering**: Easily reorder songs within playlists
- **MP3 Upload & Validation**: Upload MP3 files with format validation
- **Theme Toggle**: Switch between light and dark Bootstrap themes
- **HTML5 Audio Player**: Modern player with auto-advance functionality

## Project Structure

```
music-manager/
├── .env                      # Admin credentials (edit this!)
├── Makefile                  # Build configuration
├── fortran_fcgi.f90          # Main controller (routing & HTML)
├── db_module.f90             # SQLite database operations
├── auth_module.f90           # Authentication & sessions
├── env_module.f90            # .env file parser
├── mp3_module.f90            # MP3 validation
├── nginx.conf                # Nginx configuration
├── install.sh                # Installation script
├── restart.sh                # Quick restart for development
├── music-manager.service     # Systemd service file
├── static/
│   ├── css/style.css         # Custom styles
│   └── js/
│       ├── theme.js          # Dark/light theme toggle
│       ├── player.js         # Audio player logic
│       └── admin.js          # Admin drag-drop reordering
├── mp3/                      # Uploaded MP3 files
└── music.db                  # SQLite database (created on first run)
```

## Prerequisites

- Linux server (Ubuntu/Debian recommended)
- GFortran compiler
- Nginx web server
- SQLite3 development libraries
- FastCGI development libraries

## Quick Installation (Ubuntu/Debian)

```bash
# Clone or copy the project
cd /path/to/music-manager

# Run the installation script
sudo ./install.sh
```

## Manual Installation

### Step 1: Install Dependencies

```bash
sudo apt-get update
sudo apt-get install -y gfortran libfcgi-dev libsqlite3-dev nginx spawn-fcgi
```

### Step 2: Create Application User

```bash
sudo adduser fortran --gecos "" --disabled-password
```

### Step 3: Set Up Application Directory

```bash
sudo mkdir -p /home/fortran/music-manager
sudo cp -r ./* /home/fortran/music-manager/
sudo chown -R fortran:fortran /home/fortran/music-manager
cd /home/fortran/music-manager
```

### Step 4: Configure Credentials

Edit the `.env` file with your own credentials:

```bash
sudo -u fortran nano /home/fortran/music-manager/.env
```

```
ADMIN_USER=your_username
ADMIN_PASS=your_secure_password
SESSION_SECRET=your_random_secret_key
```

### Step 5: Compile the Application

```bash
cd /home/fortran/music-manager
sudo -u fortran make
```

### Step 6: Configure Nginx

Edit the `nginx.conf` file to set your server name:

```bash
sudo nano /home/fortran/music-manager/nginx.conf
```

Change `your-domain.com` to your actual domain or IP address.

Then install the configuration:

```bash
sudo cp /home/fortran/music-manager/nginx.conf /etc/nginx/sites-available/music-manager
sudo ln -s /etc/nginx/sites-available/music-manager /etc/nginx/sites-enabled/
sudo rm /etc/nginx/sites-enabled/default  # Remove default site
sudo nginx -t  # Test configuration
sudo systemctl restart nginx
```

### Step 7: Start the Application

**For testing:**

```bash
cd /home/fortran/music-manager
spawn-fcgi -a 127.0.0.1 -p 9000 -u fortran -g fortran ./fortran_fcgi
```

**For production (using systemd):**

```bash
sudo cp /home/fortran/music-manager/music-manager.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable music-manager
sudo systemctl start music-manager
```

## Usage

### Access the Application

- **Public Frontend**: `http://your-domain.com/`
- **Admin Login**: `http://your-domain.com/admin/login`

### Default Credentials

- Username: `admin`
- Password: `changeme123`

**⚠️ Important: Change these credentials in the `.env` file before deploying to production!**

### Admin Functions

1. **Create Playlist**: Click "New Playlist" on the dashboard
2. **Edit Playlist**: Click the pencil icon next to a playlist
3. **Upload Songs**: In the playlist edit page, use the upload form
4. **Reorder Songs**: Drag songs by the grip handle to reorder
5. **Delete Songs**: Click the X button next to a song
6. **Delete Playlist**: Click the trash icon next to a playlist

### Theme Toggle

Click the moon/sun icon in the navigation bar to switch between light and dark themes. Your preference is saved in the browser.

## Development

### Recompile and Restart

```bash
cd /home/fortran/music-manager
./restart.sh
```

### View Logs

```bash
# Nginx access log
sudo tail -f /var/log/nginx/music-manager-access.log

# Nginx error log
sudo tail -f /var/log/nginx/music-manager-error.log

# Application service status
sudo systemctl status music-manager
```

### Database

The SQLite database is created automatically at `music.db`. To reset:

```bash
rm /home/fortran/music-manager/music.db
sudo systemctl restart music-manager
```

## Troubleshooting

### Application won't start

1. Check if the application compiled successfully:
   ```bash
   cd /home/fortran/music-manager && make
   ```

2. Check if port 9000 is available:
   ```bash
   sudo netstat -tlnp | grep 9000
   ```

3. Check permissions:
   ```bash
   ls -la /home/fortran/music-manager/
   ```

### 502 Bad Gateway

The FastCGI application is not running. Start it:

```bash
spawn-fcgi -a 127.0.0.1 -p 9000 -u fortran -g fortran /home/fortran/music-manager/fortran_fcgi
```

### Permission Denied on MP3 Upload

```bash
sudo chown -R fortran:fortran /home/fortran/music-manager/mp3
sudo chmod 755 /home/fortran/music-manager/mp3
```

## Technical Notes

- **FastCGI**: The application uses FastCGI for communication between Nginx and the Fortran executable
- **SQLite**: Database operations use C bindings via ISO_C_BINDING
- **Session Management**: Simple in-memory session with cookie-based tokens
- **MP3 Validation**: Checks for ID3 tags or MP3 frame sync bytes

## Security Considerations

1. Always change default credentials in `.env`
2. Use HTTPS in production (Let's Encrypt recommended)
3. Keep the `.env` file secure with proper permissions
4. Regularly update system packages
5. Consider adding rate limiting in Nginx

## License

This project is provided as-is for educational purposes.
