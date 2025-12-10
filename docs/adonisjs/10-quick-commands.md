# Quick Commands Reference

## Development

\`\`\`bash
# Start development server
node ace serve --watch

# Run migrations
node ace migration:run

# Rollback migrations
node ace migration:rollback

# Create new migration
node ace make:migration <name>

# Create new model
node ace make:model <name>

# Create new controller
node ace make:controller <name>

# Create new middleware
node ace make:middleware <name>
\`\`\`

## Production

\`\`\`bash
# Build for production
node ace build --production

# Start production server
cd build && node server.js

# Using PM2
pm2 start build/server.js --name "music-playlist"
pm2 logs music-playlist
pm2 restart music-playlist
pm2 stop music-playlist
pm2 delete music-playlist
\`\`\`

## Nginx Configuration (Optional)

File: `/etc/nginx/sites-available/music-playlist`

\`\`\`nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:3333;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_cache_bypass $http_upgrade;
    }

    # Serve static files directly
    location /mp3 {
        alias /path/to/music-playlist/public/mp3;
        expires 30d;
        add_header Cache-Control "public, immutable";
    }
}
\`\`\`

Enable site:
\`\`\`bash
sudo ln -s /etc/nginx/sites-available/music-playlist /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
\`\`\`

## SSL with Certbot

\`\`\`bash
sudo apt install certbot python3-certbot-nginx
sudo certbot --nginx -d your-domain.com
\`\`\`

## File Structure Summary

\`\`\`
music-playlist/
├── app/
│   ├── Controllers/Http/
│   │   ├── AuthController.ts      # Login/Logout
│   │   ├── PlaylistController.ts  # CRUD playlists
│   │   ├── SongController.ts      # Delete/Update songs
│   │   └── FrontendController.ts  # Public pages
│   ├── Middleware/
│   │   └── AdminAuth.ts           # Protect admin routes
│   └── Models/
│       ├── Playlist.ts
│       └── Song.ts
├── database/migrations/
├── public/mp3/                    # Uploaded MP3 files
├── resources/views/
│   ├── layouts/
│   │   ├── admin.edge
│   │   └── frontend.edge
│   ├── admin/
│   │   ├── login.edge
│   │   ├── dashboard.edge
│   │   └── playlists/
│   │       ├── index.edge
│   │       ├── create.edge
│   │       └── edit.edge
│   └── frontend/
│       ├── home.edge
│       └── playlist.edge
├── start/routes.ts
└── .env
