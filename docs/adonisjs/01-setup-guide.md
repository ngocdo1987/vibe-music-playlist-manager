# Music Playlist Webapp - AdonisJS Setup Guide

## Project Structure

\`\`\`
music-playlist/
├── app/
│   ├── Controllers/Http/
│   │   ├── AuthController.ts
│   │   ├── PlaylistController.ts
│   │   ├── SongController.ts
│   │   └── FrontendController.ts
│   ├── Middleware/
│   │   └── AdminAuth.ts
│   ├── Models/
│   │   ├── Playlist.ts
│   │   └── Song.ts
│   └── Validators/
│       └── PlaylistValidator.ts
├── database/
│   └── migrations/
│       ├── xxxx_create_playlists_table.ts
│       └── xxxx_create_songs_table.ts
├── public/
│   └── mp3/          # Uploaded MP3 files
├── resources/
│   └── views/
│       ├── layouts/
│       │   ├── admin.edge
│       │   └── frontend.edge
│       ├── admin/
│       │   ├── login.edge
│       │   ├── dashboard.edge
│       │   ├── playlists/
│       │   │   ├── index.edge
│       │   │   ├── create.edge
│       │   │   └── edit.edge
│       └── frontend/
│           ├── home.edge
│           └── playlist.edge
├── start/
│   └── routes.ts
├── .env
└── package.json
\`\`\`

## Step 1: Install Node.js and npm

\`\`\`bash
# Update system
sudo apt update && sudo apt upgrade -y

# Install Node.js 18+ (using NodeSource)
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt install -y nodejs

# Verify installation
node --version
npm --version
\`\`\`

## Step 2: Create AdonisJS Project

\`\`\`bash
# Create new AdonisJS project
npm init adonis-ts-app@latest music-playlist

# Choose "web" project structure when prompted
# Select the following options:
# - Project name: music-playlist
# - Project structure: web
# - Setup eslint: Yes
# - Configure webpack encore: No

cd music-playlist

# Install additional dependencies
npm install @adonisjs/lucid @adonisjs/session @adonisjs/view @adonisjs/shield
npm install better-sqlite3
npm install file-type@16.5.4
npm install uuid

# Configure packages
node ace configure @adonisjs/lucid
# Select SQLite when prompted

node ace configure @adonisjs/session
node ace configure @adonisjs/view
node ace configure @adonisjs/shield
\`\`\`

## Step 3: Configure Environment

Edit `.env` file:

\`\`\`env
PORT=3333
HOST=0.0.0.0
NODE_ENV=development
APP_KEY=your-app-key-here
DRIVE_DISK=local

# Database
DB_CONNECTION=sqlite

# Admin Credentials
ADMIN_USERNAME=admin
ADMIN_PASSWORD=your-secure-password-here

# Session
SESSION_DRIVER=cookie
\`\`\`

## Step 4: Create Database Migrations

Run the following commands to create migration files, then replace their content:

\`\`\`bash
node ace make:migration playlists
node ace make:migration songs
\`\`\`

## Step 5: Create Models

\`\`\`bash
node ace make:model Playlist
node ace make:model Song
\`\`\`

## Step 6: Create Controllers

\`\`\`bash
node ace make:controller Auth
node ace make:controller Playlist
node ace make:controller Song
node ace make:controller Frontend
\`\`\`

## Step 7: Create Middleware

\`\`\`bash
node ace make:middleware AdminAuth
\`\`\`

## Step 8: Create mp3 Directory

\`\`\`bash
mkdir -p public/mp3
chmod 755 public/mp3
\`\`\`

## Step 9: Run Migrations

\`\`\`bash
node ace migration:run
\`\`\`

## Step 10: Start Development Server

\`\`\`bash
node ace serve --watch
\`\`\`

## Step 11: Production Deployment

\`\`\`bash
# Build for production
node ace build --production

# Start production server
cd build
npm ci --production
node server.js
\`\`\`

## Using PM2 for Production

\`\`\`bash
# Install PM2
npm install -g pm2

# Start with PM2
pm2 start build/server.js --name "music-playlist"

# Auto-restart on reboot
pm2 startup
pm2 save
