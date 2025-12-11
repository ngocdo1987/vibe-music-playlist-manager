use crate::models::{Playlist, Song};
use rusqlite::{Connection, Result, params};

pub fn init_db() -> Result<Connection> {
    let conn = Connection::open("music.db")?;
    
    conn.execute_batch(
        "
        CREATE TABLE IF NOT EXISTS songs (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            filename TEXT NOT NULL UNIQUE,
            original_name TEXT NOT NULL,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        );

        CREATE TABLE IF NOT EXISTS playlists (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL,
            slug TEXT NOT NULL UNIQUE,
            description TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
        );

        CREATE TABLE IF NOT EXISTS playlist_songs (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            playlist_id INTEGER NOT NULL,
            song_id INTEGER NOT NULL,
            position INTEGER NOT NULL DEFAULT 0,
            FOREIGN KEY (playlist_id) REFERENCES playlists(id) ON DELETE CASCADE,
            FOREIGN KEY (song_id) REFERENCES songs(id) ON DELETE CASCADE,
            UNIQUE(playlist_id, song_id)
        );

        CREATE INDEX IF NOT EXISTS idx_playlist_songs_playlist ON playlist_songs(playlist_id);
        CREATE INDEX IF NOT EXISTS idx_playlist_songs_position ON playlist_songs(playlist_id, position);
        "
    )?;

    Ok(conn)
}

// Song operations
pub fn create_song(conn: &Connection, filename: &str, original_name: &str) -> Result<i64> {
    conn.execute(
        "INSERT INTO songs (filename, original_name) VALUES (?1, ?2)",
        params![filename, original_name],
    )?;
    Ok(conn.last_insert_rowid())
}

pub fn get_all_songs(conn: &Connection) -> Result<Vec<Song>> {
    let mut stmt = conn.prepare(
        "SELECT id, filename, original_name, created_at FROM songs ORDER BY original_name"
    )?;
    
    let songs = stmt.query_map([], |row| {
        Ok(Song {
            id: row.get(0)?,
            filename: row.get(1)?,
            original_name: row.get(2)?,
            created_at: row.get(3)?,
        })
    })?;

    songs.collect()
}

pub fn get_song_by_id(conn: &Connection, id: i64) -> Result<Song> {
    conn.query_row(
        "SELECT id, filename, original_name, created_at FROM songs WHERE id = ?1",
        params![id],
        |row| {
            Ok(Song {
                id: row.get(0)?,
                filename: row.get(1)?,
                original_name: row.get(2)?,
                created_at: row.get(3)?,
            })
        },
    )
}

pub fn delete_song(conn: &Connection, id: i64) -> Result<String> {
    let filename: String = conn.query_row(
        "SELECT filename FROM songs WHERE id = ?1",
        params![id],
        |row| row.get(0),
    )?;
    
    conn.execute("DELETE FROM playlist_songs WHERE song_id = ?1", params![id])?;
    conn.execute("DELETE FROM songs WHERE id = ?1", params![id])?;
    
    Ok(filename)
}

// Playlist operations
pub fn create_playlist(conn: &Connection, name: &str, slug: &str, description: Option<&str>) -> Result<i64> {
    conn.execute(
        "INSERT INTO playlists (name, slug, description) VALUES (?1, ?2, ?3)",
        params![name, slug, description],
    )?;
    Ok(conn.last_insert_rowid())
}

pub fn get_all_playlists(conn: &Connection) -> Result<Vec<Playlist>> {
    let mut stmt = conn.prepare(
        "SELECT id, name, slug, description, created_at, updated_at FROM playlists ORDER BY name"
    )?;
    
    let playlists = stmt.query_map([], |row| {
        Ok(Playlist {
            id: row.get(0)?,
            name: row.get(1)?,
            slug: row.get(2)?,
            description: row.get(3)?,
            created_at: row.get(4)?,
            updated_at: row.get(5)?,
        })
    })?;

    playlists.collect()
}

pub fn get_playlist_by_id(conn: &Connection, id: i64) -> Result<Playlist> {
    conn.query_row(
        "SELECT id, name, slug, description, created_at, updated_at FROM playlists WHERE id = ?1",
        params![id],
        |row| {
            Ok(Playlist {
                id: row.get(0)?,
                name: row.get(1)?,
                slug: row.get(2)?,
                description: row.get(3)?,
                created_at: row.get(4)?,
                updated_at: row.get(5)?,
            })
        },
    )
}

pub fn get_playlist_by_slug(conn: &Connection, slug: &str) -> Result<Playlist> {
    conn.query_row(
        "SELECT id, name, slug, description, created_at, updated_at FROM playlists WHERE slug = ?1",
        params![slug],
        |row| {
            Ok(Playlist {
                id: row.get(0)?,
                name: row.get(1)?,
                slug: row.get(2)?,
                description: row.get(3)?,
                created_at: row.get(4)?,
                updated_at: row.get(5)?,
            })
        },
    )
}

pub fn update_playlist(conn: &Connection, id: i64, name: &str, slug: &str, description: Option<&str>) -> Result<()> {
    conn.execute(
        "UPDATE playlists SET name = ?1, slug = ?2, description = ?3, updated_at = CURRENT_TIMESTAMP WHERE id = ?4",
        params![name, slug, description, id],
    )?;
    Ok(())
}

pub fn delete_playlist(conn: &Connection, id: i64) -> Result<()> {
    conn.execute("DELETE FROM playlist_songs WHERE playlist_id = ?1", params![id])?;
    conn.execute("DELETE FROM playlists WHERE id = ?1", params![id])?;
    Ok(())
}

// Playlist songs operations
pub fn get_playlist_songs(conn: &Connection, playlist_id: i64) -> Result<Vec<Song>> {
    let mut stmt = conn.prepare(
        "SELECT s.id, s.filename, s.original_name, s.created_at 
         FROM songs s
         INNER JOIN playlist_songs ps ON s.id = ps.song_id
         WHERE ps.playlist_id = ?1
         ORDER BY ps.position"
    )?;
    
    let songs = stmt.query_map(params![playlist_id], |row| {
        Ok(Song {
            id: row.get(0)?,
            filename: row.get(1)?,
            original_name: row.get(2)?,
            created_at: row.get(3)?,
        })
    })?;

    songs.collect()
}

pub fn update_playlist_songs(conn: &Connection, playlist_id: i64, song_ids: &[i64]) -> Result<()> {
    // Remove all existing songs from playlist
    conn.execute("DELETE FROM playlist_songs WHERE playlist_id = ?1", params![playlist_id])?;
    
    // Add songs with new positions
    for (position, song_id) in song_ids.iter().enumerate() {
        conn.execute(
            "INSERT INTO playlist_songs (playlist_id, song_id, position) VALUES (?1, ?2, ?3)",
            params![playlist_id, song_id, position as i32],
        )?;
    }
    
    Ok(())
}
