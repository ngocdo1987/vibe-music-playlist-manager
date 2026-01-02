#!/bin/bash
# Restart script for development
# IMPORTANT: This script changes to the project directory before starting spawn-fcgi

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Working directory: $(pwd)"

echo "Stopping existing process..."
pkill -f fortran_fcgi 2>/dev/null || true
sleep 1

echo "Recompiling..."
make

echo "Starting FastCGI server..."
# Run from the project directory so relative paths work
spawn-fcgi -a 127.0.0.1 -p 9000 -d "$SCRIPT_DIR" ./fortran_fcgi

echo "Server started on port 9000"
echo "Access at: http://127.0.0.1/"
