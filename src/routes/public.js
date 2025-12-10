const express = require('express');
const router = express.Router();
const db = require('../config/database');

// Home page - list all playlists
router.get('/', (req, res) => {
  const playlists = db.prepare('SELECT * FROM playlists ORDER BY created_at DESC').all();
  res.render('public/player', { 
    playlists,
    currentPlaylist: null,
    songs: []
  });
});

// Playlist player page
router.get('/playlist/:slug', (req, res) => {
  const playlist = db.prepare('SELECT * FROM playlists WHERE slug = ?').get(req.params.slug);
  
  if (!playlist) {
    return res.status(404).send('Playlist not found');
  }

  const songs = db.prepare(`
    SELECT s.*, ps.position 
    FROM songs s
    JOIN playlist_songs ps ON s.id = ps.song_id
    WHERE ps.playlist_id = ?
    ORDER BY ps.position
  `).all(playlist.id);

  const playlists = db.prepare('SELECT * FROM playlists ORDER BY created_at DESC').all();

  res.render('public/player-winamp', { 
    playlists,
    currentPlaylist: playlist,
    songs
  });
});

module.exports = router;