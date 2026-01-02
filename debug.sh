#!/bin/bash
# Debug script - captures crash output to log file

cd /home/ngoc/projects/pet/fortran/music-manager

echo "Stopping existing process..."
pkill -f fortran_fcgi 2>/dev/null
sleep 1

echo "Building debug version..."
make debug

echo "Starting FastCGI server with crash logging..."
echo "Crash output will be saved to: crash.log"

# Run spawn-fcgi in non-forking mode with stderr redirected to log
spawn-fcgi -n -a 127.0.0.1 -p 9000 ./fortran_fcgi 2>&1 | tee crash.log &

echo ""
echo "Server started! Now upload your UTF-8 file."
echo "After crash, check crash.log for the error."
echo ""
echo "To view log: cat crash.log"
