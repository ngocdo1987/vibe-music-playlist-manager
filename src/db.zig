// Database module - SQLite connection using C API
const std = @import("std");
const c = @cImport({
    @cInclude("sqlite3.h");
});

pub const Database = struct {
    db: ?*c.sqlite3,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Initialize and open the database connection
    pub fn init(allocator: std.mem.Allocator, path: [:0]const u8) !Self {
        var db_ptr: ?*c.sqlite3 = null;
        const result = c.sqlite3_open(path.ptr, &db_ptr);
        if (result != c.SQLITE_OK) {
            if (db_ptr) |db| _ = c.sqlite3_close(db);
            return error.SqliteOpenFailed;
        }

        var self = Self{
            .db = db_ptr,
            .allocator = allocator,
        };

        // Enable foreign keys
        _ = self.execSimple("PRAGMA foreign_keys = ON");

        return self;
    }

    /// Close the database connection
    pub fn deinit(self: *Self) void {
        if (self.db) |db| {
            _ = c.sqlite3_close(db);
        }
    }

    /// Execute a simple SQL statement without parameters
    fn execSimple(self: *Self, sql: [*:0]const u8) bool {
        if (self.db) |db| {
            var err_msg: [*c]u8 = null;
            const result = c.sqlite3_exec(db, sql, null, null, &err_msg);
            if (err_msg != null) {
                std.debug.print("SQL Error: {s}\n", .{err_msg});
                c.sqlite3_free(err_msg);
            }
            return result == c.SQLITE_OK;
        }
        return false;
    }

    /// Create all required tables
    pub fn createTables(self: *Self) !void {
        // Playlists table
        if (!self.execSimple(
            \\CREATE TABLE IF NOT EXISTS playlists (
            \\    id INTEGER PRIMARY KEY AUTOINCREMENT,
            \\    name TEXT NOT NULL,
            \\    slug TEXT NOT NULL UNIQUE,
            \\    created_at INTEGER DEFAULT (strftime('%s', 'now')),
            \\    updated_at INTEGER DEFAULT (strftime('%s', 'now'))
            \\)
        )) return error.CreateTableFailed;

        // Songs table
        if (!self.execSimple(
            \\CREATE TABLE IF NOT EXISTS songs (
            \\    id INTEGER PRIMARY KEY AUTOINCREMENT,
            \\    title TEXT NOT NULL,
            \\    filename TEXT NOT NULL UNIQUE,
            \\    file_path TEXT NOT NULL,
            \\    created_at INTEGER DEFAULT (strftime('%s', 'now'))
            \\)
        )) return error.CreateTableFailed;

        // Playlist-Songs junction table
        if (!self.execSimple(
            \\CREATE TABLE IF NOT EXISTS playlist_songs (
            \\    id INTEGER PRIMARY KEY AUTOINCREMENT,
            \\    playlist_id INTEGER NOT NULL,
            \\    song_id INTEGER NOT NULL,
            \\    position INTEGER NOT NULL DEFAULT 0,
            \\    FOREIGN KEY (playlist_id) REFERENCES playlists(id) ON DELETE CASCADE,
            \\    FOREIGN KEY (song_id) REFERENCES songs(id) ON DELETE CASCADE,
            \\    UNIQUE(playlist_id, song_id)
            \\)
        )) return error.CreateTableFailed;

        // Create indexes
        _ = self.execSimple("CREATE INDEX IF NOT EXISTS idx_playlists_slug ON playlists(slug)");
        _ = self.execSimple("CREATE INDEX IF NOT EXISTS idx_playlist_songs_position ON playlist_songs(playlist_id, position)");
    }

    // ==================== Playlist Operations ====================

    pub const Playlist = struct {
        id: i64,
        name: []const u8,
        slug: []const u8,
        created_at: i64,
        updated_at: i64,
    };

    /// Result container for playlists - using fixed array
    pub const PlaylistList = struct {
        items: []Playlist,
        allocator: std.mem.Allocator,

        pub fn deinit(self: *PlaylistList) void {
            self.allocator.free(self.items);
        }
    };

    /// Get all playlists
    pub fn getAllPlaylists(self: *Self, allocator: std.mem.Allocator) !PlaylistList {
        const sql = "SELECT id, name, slug, created_at, updated_at FROM playlists ORDER BY created_at DESC";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        // First pass: count rows
        var count: usize = 0;
        while (c.sqlite3_step(stmt) == c.SQLITE_ROW) {
            count += 1;
        }

        // Reset and allocate
        _ = c.sqlite3_reset(stmt);
        const items = try allocator.alloc(Playlist, count);
        errdefer allocator.free(items);

        // Second pass: fill data
        var i: usize = 0;
        while (c.sqlite3_step(stmt) == c.SQLITE_ROW) : (i += 1) {
            const name_ptr: [*c]const u8 = @ptrCast(c.sqlite3_column_text(stmt, 1));
            const slug_ptr: [*c]const u8 = @ptrCast(c.sqlite3_column_text(stmt, 2));

            items[i] = .{
                .id = c.sqlite3_column_int64(stmt, 0),
                .name = try allocator.dupe(u8, std.mem.span(name_ptr)),
                .slug = try allocator.dupe(u8, std.mem.span(slug_ptr)),
                .created_at = c.sqlite3_column_int64(stmt, 3),
                .updated_at = c.sqlite3_column_int64(stmt, 4),
            };
        }

        return PlaylistList{ .items = items, .allocator = allocator };
    }

    /// Get playlist by slug
    pub fn getPlaylistBySlug(self: *Self, allocator: std.mem.Allocator, slug: []const u8) !?Playlist {
        const sql = "SELECT id, name, slug, created_at, updated_at FROM playlists WHERE slug = ?";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        // Create null-terminated version of slug for binding
        const slug_z = try allocator.dupeZ(u8, slug);
        defer allocator.free(slug_z);

        if (c.sqlite3_bind_text(stmt, 1, slug_z.ptr, @intCast(slug.len), null) != c.SQLITE_OK) {
            return error.BindFailed;
        }

        if (c.sqlite3_step(stmt) == c.SQLITE_ROW) {
            const name_ptr: [*c]const u8 = @ptrCast(c.sqlite3_column_text(stmt, 1));
            const slug_result_ptr: [*c]const u8 = @ptrCast(c.sqlite3_column_text(stmt, 2));

            return Playlist{
                .id = c.sqlite3_column_int64(stmt, 0),
                .name = try allocator.dupe(u8, std.mem.span(name_ptr)),
                .slug = try allocator.dupe(u8, std.mem.span(slug_result_ptr)),
                .created_at = c.sqlite3_column_int64(stmt, 3),
                .updated_at = c.sqlite3_column_int64(stmt, 4),
            };
        }

        return null;
    }

    /// Create a new playlist and return its ID
    pub fn createPlaylist(self: *Self, allocator: std.mem.Allocator, name: []const u8, slug: []const u8) !i64 {
        const sql = "INSERT INTO playlists (name, slug) VALUES (?, ?)";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        const name_z = try allocator.dupeZ(u8, name);
        defer allocator.free(name_z);
        const slug_z = try allocator.dupeZ(u8, slug);
        defer allocator.free(slug_z);

        _ = c.sqlite3_bind_text(stmt, 1, name_z.ptr, @intCast(name.len), null);
        _ = c.sqlite3_bind_text(stmt, 2, slug_z.ptr, @intCast(slug.len), null);

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return error.InsertFailed;
        }

        return c.sqlite3_last_insert_rowid(self.db);
    }

    /// Delete a playlist
    pub fn deletePlaylist(self: *Self, id: i64) !void {
        const sql = "DELETE FROM playlists WHERE id = ?";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        _ = c.sqlite3_bind_int64(stmt, 1, id);

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return error.DeleteFailed;
        }
    }

    // ==================== Song Operations ====================

    pub const Song = struct {
        id: i64,
        title: []const u8,
        filename: []const u8,
        file_path: []const u8,
        created_at: i64,
    };

    /// Result container for songs
    pub const SongList = struct {
        items: []Song,
        allocator: std.mem.Allocator,

        pub fn deinit(self: *SongList) void {
            self.allocator.free(self.items);
        }
    };

    /// Create a new song and return its ID
    pub fn createSong(self: *Self, allocator: std.mem.Allocator, title: []const u8, filename: []const u8, file_path: []const u8) !i64 {
        const sql = "INSERT INTO songs (title, filename, file_path) VALUES (?, ?, ?)";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        const title_z = try allocator.dupeZ(u8, title);
        defer allocator.free(title_z);
        const filename_z = try allocator.dupeZ(u8, filename);
        defer allocator.free(filename_z);
        const file_path_z = try allocator.dupeZ(u8, file_path);
        defer allocator.free(file_path_z);

        _ = c.sqlite3_bind_text(stmt, 1, title_z.ptr, @intCast(title.len), null);
        _ = c.sqlite3_bind_text(stmt, 2, filename_z.ptr, @intCast(filename.len), null);
        _ = c.sqlite3_bind_text(stmt, 3, file_path_z.ptr, @intCast(file_path.len), null);

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return error.InsertFailed;
        }

        return c.sqlite3_last_insert_rowid(self.db);
    }

    /// Get songs for a playlist ordered by position
    pub fn getSongsForPlaylist(self: *Self, allocator: std.mem.Allocator, playlist_id: i64) !SongList {
        const sql =
            \\SELECT s.id, s.title, s.filename, s.file_path, s.created_at
            \\FROM songs s
            \\JOIN playlist_songs ps ON s.id = ps.song_id
            \\WHERE ps.playlist_id = ?
            \\ORDER BY ps.position ASC
        ;
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        _ = c.sqlite3_bind_int64(stmt, 1, playlist_id);

        // First pass: count rows
        var count: usize = 0;
        while (c.sqlite3_step(stmt) == c.SQLITE_ROW) {
            count += 1;
        }

        // Reset and allocate
        _ = c.sqlite3_reset(stmt);
        _ = c.sqlite3_bind_int64(stmt, 1, playlist_id);
        const items = try allocator.alloc(Song, count);
        errdefer allocator.free(items);

        // Second pass: fill data
        var i: usize = 0;
        while (c.sqlite3_step(stmt) == c.SQLITE_ROW) : (i += 1) {
            const title_ptr: [*c]const u8 = @ptrCast(c.sqlite3_column_text(stmt, 1));
            const filename_ptr: [*c]const u8 = @ptrCast(c.sqlite3_column_text(stmt, 2));
            const file_path_ptr: [*c]const u8 = @ptrCast(c.sqlite3_column_text(stmt, 3));

            items[i] = .{
                .id = c.sqlite3_column_int64(stmt, 0),
                .title = try allocator.dupe(u8, std.mem.span(title_ptr)),
                .filename = try allocator.dupe(u8, std.mem.span(filename_ptr)),
                .file_path = try allocator.dupe(u8, std.mem.span(file_path_ptr)),
                .created_at = c.sqlite3_column_int64(stmt, 4),
            };
        }

        return SongList{ .items = items, .allocator = allocator };
    }

    /// Add song to playlist
    pub fn addSongToPlaylist(self: *Self, playlist_id: i64, song_id: i64, position: i32) !void {
        const sql = "INSERT INTO playlist_songs (playlist_id, song_id, position) VALUES (?, ?, ?)";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        _ = c.sqlite3_bind_int64(stmt, 1, playlist_id);
        _ = c.sqlite3_bind_int64(stmt, 2, song_id);
        _ = c.sqlite3_bind_int(stmt, 3, position);

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return error.InsertFailed;
        }
    }

    /// Update song position
    pub fn updateSongPosition(self: *Self, playlist_id: i64, song_id: i64, position: i32) !void {
        const sql = "UPDATE playlist_songs SET position = ? WHERE playlist_id = ? AND song_id = ?";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        _ = c.sqlite3_bind_int(stmt, 1, position);
        _ = c.sqlite3_bind_int64(stmt, 2, playlist_id);
        _ = c.sqlite3_bind_int64(stmt, 3, song_id);

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return error.UpdateFailed;
        }
    }

    /// Update a playlist's name and slug
    pub fn updatePlaylist(self: *Self, id: i64, name: []const u8, slug: []const u8) !void {
        const sql = "UPDATE playlists SET name = ?, slug = ?, updated_at = strftime('%s', 'now') WHERE id = ?";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        var name_buf: [256]u8 = undefined;
        var slug_buf: [256]u8 = undefined;
        @memcpy(name_buf[0..name.len], name);
        name_buf[name.len] = 0;
        @memcpy(slug_buf[0..slug.len], slug);
        slug_buf[slug.len] = 0;

        _ = c.sqlite3_bind_text(stmt, 1, &name_buf, @intCast(name.len), null);
        _ = c.sqlite3_bind_text(stmt, 2, &slug_buf, @intCast(slug.len), null);
        _ = c.sqlite3_bind_int64(stmt, 3, id);

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return error.UpdateFailed;
        }
    }

    /// Remove a song from a playlist
    pub fn removeSongFromPlaylist(self: *Self, playlist_id: i64, song_id: i64) !void {
        const sql = "DELETE FROM playlist_songs WHERE playlist_id = ? AND song_id = ?";
        var stmt: ?*c.sqlite3_stmt = null;

        if (c.sqlite3_prepare_v2(self.db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return error.PrepareFailed;
        }
        defer _ = c.sqlite3_finalize(stmt);

        _ = c.sqlite3_bind_int64(stmt, 1, playlist_id);
        _ = c.sqlite3_bind_int64(stmt, 2, song_id);

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return error.DeleteFailed;
        }
    }
};

// ==================== Helper Functions ====================

/// Generate a URL-safe slug from a string
pub fn generateSlug(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    // Count needed characters first
    var count: usize = 0;
    var last_was_dash = true; // Start true to skip leading dashes
    for (name) |char| {
        if (std.ascii.isAlphanumeric(char)) {
            count += 1;
            last_was_dash = false;
        } else if ((char == ' ' or char == '-' or char == '_') and !last_was_dash) {
            count += 1;
            last_was_dash = true;
        }
    }

    // Trim trailing dash
    if (count > 0 and last_was_dash) count -= 1;

    const result = try allocator.alloc(u8, count);

    var i: usize = 0;
    last_was_dash = true;
    for (name) |char| {
        if (std.ascii.isAlphanumeric(char)) {
            result[i] = std.ascii.toLower(char);
            i += 1;
            last_was_dash = false;
        } else if ((char == ' ' or char == '-' or char == '_') and !last_was_dash) {
            result[i] = '-';
            i += 1;
            last_was_dash = true;
        }
    }

    return result[0..i];
}

test "generateSlug basic" {
    const allocator = std.testing.allocator;
    const slug = try generateSlug(allocator, "My Awesome Playlist");
    defer allocator.free(slug);
    try std.testing.expectEqualStrings("my-awesome-playlist", slug);
}

test "generateSlug special chars" {
    const allocator = std.testing.allocator;
    const slug = try generateSlug(allocator, "Rock & Roll 2024!");
    defer allocator.free(slug);
    try std.testing.expectEqualStrings("rock-roll-2024", slug);
}
