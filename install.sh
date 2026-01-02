#!/bin/bash
# Installation script for Fortran Music Manager on Ubuntu/Debian

set -e

echo "=== Fortran Music Manager Installation ==="
echo ""

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo "Please run as root (sudo ./install.sh)"
    exit 1
fi

# Update system
echo "[1/6] Updating system packages..."
apt-get update
apt-get upgrade -y

# Install dependencies
echo "[2/6] Installing dependencies..."
apt-get install -y \
    gfortran \
    libfcgi-dev \
    libsqlite3-dev \
    nginx \
    spawn-fcgi \
    git

# Create fortran user if not exists
echo "[3/6] Setting up user..."
if ! id "fortran" &>/dev/null; then
    adduser fortran --gecos "" --disabled-password
    echo "Created user: fortran"
fi

# Set up application directory
APP_DIR="/home/fortran/music-manager"
echo "[4/6] Setting up application at $APP_DIR..."

if [ -d "$APP_DIR" ]; then
    echo "Directory exists, updating..."
else
    mkdir -p "$APP_DIR"
fi

# Copy files (assuming running from source directory)
cp -r ./* "$APP_DIR/" 2>/dev/null || true
chown -R fortran:fortran "$APP_DIR"
chmod 755 "$APP_DIR"
chmod 755 "$APP_DIR/mp3"

# Compile the application
echo "[5/6] Compiling Fortran application..."
cd "$APP_DIR"
sudo -u fortran make clean
sudo -u fortran make

# Set up Nginx
echo "[6/6] Configuring Nginx..."
cp "$APP_DIR/nginx.conf" /etc/nginx/sites-available/music-manager

# Update server_name in config
read -p "Enter your domain or IP address: " DOMAIN
sed -i "s/your-domain.com/$DOMAIN/" /etc/nginx/sites-available/music-manager

# Enable site
ln -sf /etc/nginx/sites-available/music-manager /etc/nginx/sites-enabled/

# Remove default if exists
rm -f /etc/nginx/sites-enabled/default

# Test and restart Nginx
nginx -t && systemctl restart nginx

echo ""
echo "=== Installation Complete ==="
echo ""
echo "To start the application, run:"
echo "  cd $APP_DIR"
echo "  spawn-fcgi -a 127.0.0.1 -p 9000 -u fortran -g fortran ./fortran_fcgi"
echo ""
echo "Or use the provided systemd service:"
echo "  sudo cp $APP_DIR/music-manager.service /etc/systemd/system/"
echo "  sudo systemctl enable music-manager"
echo "  sudo systemctl start music-manager"
echo ""
echo "Default admin credentials (change in .env):"
echo "  Username: admin"
echo "  Password: changeme123"
echo ""
echo "Access the application at: http://$DOMAIN"
