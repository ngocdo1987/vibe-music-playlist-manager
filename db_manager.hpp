#pragma once
#include <sqlite3.h>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

struct Song {
    int id;
    std::string title;
    std::string filename;
};

struct Playlist {
    int id;
    std::string name;
    std::string description;
};

class DBManager {
public:
    DBManager(const std::string& db_path) {
        if (sqlite3_open(db_path.c_str(), &db) != SQLITE_OK) {
            throw std::runtime_error("Can't open database: " + std::string(sqlite3_errmsg(db)));
        }
        init_tables();
    }

    ~DBManager() {
        sqlite3_close(db);
    }

    void init_tables() {
        const char* sql = 
            "CREATE TABLE IF NOT EXISTS songs ("
            "id INTEGER PRIMARY KEY AUTOINCREMENT,"
            "title TEXT NOT NULL,"
            "filename TEXT NOT NULL"
            ");"
            "CREATE TABLE IF NOT EXISTS playlists ("
            "id INTEGER PRIMARY KEY AUTOINCREMENT,"
            "name TEXT NOT NULL,"
            "description TEXT"
            ");"
            "CREATE TABLE IF NOT EXISTS playlist_songs ("
            "playlist_id INTEGER,"
            "song_id INTEGER,"
            "position INTEGER,"
            "PRIMARY KEY (playlist_id, song_id),"
            "FOREIGN KEY (playlist_id) REFERENCES playlists(id) ON DELETE CASCADE,"
            "FOREIGN KEY (song_id) REFERENCES songs(id) ON DELETE CASCADE"
            ");";
        
        char* zErrMsg = 0;
        if (sqlite3_exec(db, sql, 0, 0, &zErrMsg) != SQLITE_OK) {
            std::string err = zErrMsg;
            sqlite3_free(zErrMsg);
            throw std::runtime_error("SQL error: " + err);
        }
    }

    // Playlist operations
    int create_playlist(const std::string& name, const std::string& description) {
        sqlite3_stmt* stmt;
        const char* sql = "INSERT INTO playlists (name, description) VALUES (?, ?);";
        sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
        sqlite3_bind_text(stmt, 1, name.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 2, description.c_str(), -1, SQLITE_STATIC);
        
        if (sqlite3_step(stmt) != SQLITE_DONE) {
            sqlite3_finalize(stmt);
            return -1;
        }
        int id = (int)sqlite3_last_insert_rowid(db);
        sqlite3_finalize(stmt);
        return id;
    }

    std::vector<Playlist> get_playlists() {
        std::vector<Playlist> list;
        sqlite3_stmt* stmt;
        sqlite3_prepare_v2(db, "SELECT id, name, description FROM playlists;", -1, &stmt, 0);
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            list.push_back({
                sqlite3_column_int(stmt, 0),
                (const char*)sqlite3_column_text(stmt, 1),
                sqlite3_column_text(stmt, 2) ? (const char*)sqlite3_column_text(stmt, 2) : ""
            });
        }
        sqlite3_finalize(stmt);
        return list;
    }

    Playlist get_playlist(int id) {
        sqlite3_stmt* stmt;
        sqlite3_prepare_v2(db, "SELECT id, name, description FROM playlists WHERE id = ?;", -1, &stmt, 0);
        sqlite3_bind_int(stmt, 1, id);
        Playlist p = {-1, "", ""};
        if (sqlite3_step(stmt) == SQLITE_ROW) {
            p = {
                sqlite3_column_int(stmt, 0),
                (const char*)sqlite3_column_text(stmt, 1),
                sqlite3_column_text(stmt, 2) ? (const char*)sqlite3_column_text(stmt, 2) : ""
            };
        }
        sqlite3_finalize(stmt);
        return p;
    }

    void delete_playlist(int id) {
        sqlite3_stmt* stmt;
        sqlite3_prepare_v2(db, "DELETE FROM playlists WHERE id = ?;", -1, &stmt, 0);
        sqlite3_bind_int(stmt, 1, id);
        sqlite3_step(stmt);
        sqlite3_finalize(stmt);
    }

    // Song operations
    int add_song(const std::string& title, const std::string& filename) {
        sqlite3_stmt* stmt;
        sqlite3_prepare_v2(db, "INSERT INTO songs (title, filename) VALUES (?, ?);", -1, &stmt, 0);
        sqlite3_bind_text(stmt, 1, title.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 2, filename.c_str(), -1, SQLITE_STATIC);
        sqlite3_step(stmt);
        int id = (int)sqlite3_last_insert_rowid(db);
        sqlite3_finalize(stmt);
        return id;
    }

    void add_song_to_playlist(int playlist_id, int song_id, int position) {
        sqlite3_stmt* stmt;
        sqlite3_prepare_v2(db, "INSERT INTO playlist_songs (playlist_id, song_id, position) VALUES (?, ?, ?);", -1, &stmt, 0);
        sqlite3_bind_int(stmt, 1, playlist_id);
        sqlite3_bind_int(stmt, 2, song_id);
        sqlite3_bind_int(stmt, 3, position);
        sqlite3_step(stmt);
        sqlite3_finalize(stmt);
    }

    void clear_playlist_songs(int playlist_id) {
        sqlite3_stmt* stmt;
        sqlite3_prepare_v2(db, "DELETE FROM playlist_songs WHERE playlist_id = ?;", -1, &stmt, 0);
        sqlite3_bind_int(stmt, 1, playlist_id);
        sqlite3_step(stmt);
        sqlite3_finalize(stmt);
    }

    std::vector<Song> get_playlist_songs(int playlist_id) {
        std::vector<Song> list;
        sqlite3_stmt* stmt;
        const char* sql = 
            "SELECT s.id, s.title, s.filename FROM songs s "
            "JOIN playlist_songs ps ON s.id = ps.song_id "
            "WHERE ps.playlist_id = ? "
            "ORDER BY ps.position ASC;";
        sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
        sqlite3_bind_int(stmt, 1, playlist_id);
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            list.push_back({
                sqlite3_column_int(stmt, 0),
                (const char*)sqlite3_column_text(stmt, 1),
                (const char*)sqlite3_column_text(stmt, 2)
            });
        }
        sqlite3_finalize(stmt);
        return list;
    }

    void delete_orphaned_songs(const std::vector<int>& song_ids, std::vector<std::string>& files_to_delete) {
        for (int sid : song_ids) {
            sqlite3_stmt* stmt;
            sqlite3_prepare_v2(db, "SELECT COUNT(*) FROM playlist_songs WHERE song_id = ?;", -1, &stmt, 0);
            sqlite3_bind_int(stmt, 1, sid);
            int count = 0;
            if (sqlite3_step(stmt) == SQLITE_ROW) {
                count = sqlite3_column_int(stmt, 0);
            }
            sqlite3_finalize(stmt);

            if (count == 0) {
                // Get filename first
                sqlite3_prepare_v2(db, "SELECT filename FROM songs WHERE id = ?;", -1, &stmt, 0);
                sqlite3_bind_int(stmt, 1, sid);
                if (sqlite3_step(stmt) == SQLITE_ROW) {
                    files_to_delete.push_back((const char*)sqlite3_column_text(stmt, 0));
                }
                sqlite3_finalize(stmt);

                // Delete from songs
                sqlite3_prepare_v2(db, "DELETE FROM songs WHERE id = ?;", -1, &stmt, 0);
                sqlite3_bind_int(stmt, 1, sid);
                sqlite3_step(stmt);
                sqlite3_finalize(stmt);
            }
        }
    }

private:
    sqlite3* db;
};
