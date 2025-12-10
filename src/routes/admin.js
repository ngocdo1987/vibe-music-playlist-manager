const express = require('express');
const router = express.Router();
const path = require('path');
const fs = require('fs');
const db = require('../config/database');
const { isAuthenticated } = require('../middleware/auth');

// Login page
router.get('/login', (req, res) => {
  if (req.session && req.session.isAdmin) {
    return res.redirect('/admin/dashboard');
  }
  res.render('admin/login', { error: null });
});

// Login handler
router.post('/login', (req, res) => {
  const { username, password } = req.body;
  
  if (username === process.env.ADMIN_USERNAME && password === process.env.ADMIN_PASSWORD) {
    req.session.isAdmin = true;
    res.redirect('/admin/dashboard');
  } else {
    res.render('admin/login', { error: 'Invalid username or password' });
  }
});

// Logout
router.get('/logout', (req, res) => {
  req.session.destroy();
  res.redirect('/admin/login');
});

// Dashboard
router.get('/dashboard', isAuthenticated, (req, res) => {
  const playlists = db.prepare('SELECT * FROM playlists ORDER BY created_at DESC').all();
  res.render('admin/dashboard', { playlists });
});

// Create playlist page
router.get('/playlist/new', isAuthenticated, (req, res) => {
  res.render('admin/playlist-form', { 
    playlist: null, 
    songs: [],
    mode: 'create'
  });
});

// Edit playlist page
router.get('/playlist/edit/:id', isAuthenticated, (req, res) => {
  const playlist = db.prepare('SELECT * FROM playlists WHERE id = ?').get(req.params.id);
  
  if (!playlist) {
    return res.redirect('/admin/dashboard');
  }

  const songs = db.prepare(`
    SELECT s.*, ps.position 
    FROM songs s
    JOIN playlist_songs ps ON s.id = ps.song_id
    WHERE ps.playlist_id = ?
    ORDER BY ps.position
  `).all(req.params.id);

  res.render('admin/playlist-form', { 
    playlist, 
    songs,
    mode: 'edit'
  });
});

// Create playlist
router.post('/playlist/create', isAuthenticated, (req, res) => {
  const { name, description, slug } = req.body;
  
  try {
    const insert = db.prepare('INSERT INTO playlists (name, description, slug) VALUES (?, ?, ?)');
    const result = insert.run(name, description, slug);
    
    res.json({ success: true, playlistId: result.lastInsertRowid });
  } catch (error) {
    res.status(400).json({ success: false, error: error.message });
  }
});

// Update playlist
router.post('/playlist/update/:id', isAuthenticated, (req, res) => {
  const { name, description, slug } = req.body;
  
  try {
    const update = db.prepare('UPDATE playlists SET name = ?, description = ?, slug = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?');
    update.run(name, description, slug, req.params.id);
    
    res.json({ success: true });
  } catch (error) {
    res.status(400).json({ success: false, error: error.message });
  }
});

// Delete playlist
router.post('/playlist/delete/:id', isAuthenticated, (req, res) => {
  try {
    // Get all songs in the playlist
    const songs = db.prepare(`
      SELECT s.* FROM songs s
      JOIN playlist_songs ps ON s.id = ps.song_id
      WHERE ps.playlist_id = ?
    `).all(req.params.id);

    // Delete playlist (cascade will handle playlist_songs)
    db.prepare('DELETE FROM playlists WHERE id = ?').run(req.params.id);

    // Check if songs are used in other playlists, delete if not
    songs.forEach(song => {
      const usageCount = db.prepare('SELECT COUNT(*) as count FROM playlist_songs WHERE song_id = ?').get(song.id);
      if (usageCount.count === 0) {
        // Delete file
        const filePath = path.join(__dirname, '../../', song.file_path);
        if (fs.existsSync(filePath)) {
          fs.unlinkSync(filePath);
        }
        // Delete from database
        db.prepare('DELETE FROM songs WHERE id = ?').run(song.id);
      }
    });

    res.json({ success: true });
  } catch (error) {
    res.status(400).json({ success: false, error: error.message });
  }
});

// Upload song
router.post('/song/upload', isAuthenticated, (req, res) => {
  if (!req.files || !req.files.song) {
    return res.status(400).json({ success: false, error: 'No file uploaded' });
  }

  const file = req.files.song;
  
  // Validate MP3 file
  if (!file.name.toLowerCase().endsWith('.mp3')) {
    return res.status(400).json({ success: false, error: 'Only MP3 files are allowed' });
  }

  // Generate unique filename
  const timestamp = Date.now();
  const filename = `${timestamp}_${file.name.replace(/[^a-zA-Z0-9._-]/g, '_')}`;
  const uploadPath = path.join(__dirname, '../../mp3', filename);

  // Move file
  file.mv(uploadPath, (err) => {
    if (err) {
      return res.status(500).json({ success: false, error: err.message });
    }

    // Save to database
    const insert = db.prepare('INSERT INTO songs (filename, original_name, file_path, file_size) VALUES (?, ?, ?, ?)');
    const result = insert.run(filename, file.name, `mp3/${filename}`, file.size);

    res.json({ 
      success: true, 
      song: {
        id: result.lastInsertRowid,
        filename,
        original_name: file.name,
        file_path: `mp3/${filename}`
      }
    });
  });
});

// Add song to playlist
router.post('/playlist/:id/add-song', isAuthenticated, (req, res) => {
  const { songId, position } = req.body;
  
  try {
    const insert = db.prepare('INSERT INTO playlist_songs (playlist_id, song_id, position) VALUES (?, ?, ?)');
    insert.run(req.params.id, songId, position);
    
    res.json({ success: true });
  } catch (error) {
    res.status(400).json({ success: false, error: error.message });
  }
});

// Remove song from playlist
router.post('/playlist/:playlistId/remove-song/:songId', isAuthenticated, (req, res) => {
  try {
    db.prepare('DELETE FROM playlist_songs WHERE playlist_id = ? AND song_id = ?')
      .run(req.params.playlistId, req.params.songId);
    
    // Check if song is used in other playlists
    const usageCount = db.prepare('SELECT COUNT(*) as count FROM playlist_songs WHERE song_id = ?')
      .get(req.params.songId);
    
    if (usageCount.count === 0) {
      const song = db.prepare('SELECT * FROM songs WHERE id = ?').get(req.params.songId);
      if (song) {
        // Delete file
        const filePath = path.join(__dirname, '../../', song.file_path);
        if (fs.existsSync(filePath)) {
          fs.unlinkSync(filePath);
        }
        // Delete from database
        db.prepare('DELETE FROM songs WHERE id = ?').run(req.params.songId);
      }
    }
    
    res.json({ success: true });
  } catch (error) {
    res.status(400).json({ success: false, error: error.message });
  }
});

// Update song positions
router.post('/playlist/:id/reorder', isAuthenticated, (req, res) => {
  const { songIds } = req.body;
  
  try {
    const updatePosition = db.prepare('UPDATE playlist_songs SET position = ? WHERE playlist_id = ? AND song_id = ?');
    
    songIds.forEach((songId, index) => {
      updatePosition.run(index, req.params.id, songId);
    });
    
    res.json({ success: true });
  } catch (error) {
    res.status(400).json({ success: false, error: error.message });
  }
});

module.exports = router;