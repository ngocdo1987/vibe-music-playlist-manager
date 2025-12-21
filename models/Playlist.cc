#include "models/Playlist.h"
#include <drogon/orm/DbClient.h>

using namespace models;
using namespace drogon::orm;

Playlist::Playlist(int id, const std::string& name, const std::string& description)
    : id_(id), name_(name), description_(description) {}

bool Playlist::create(const Playlist& playlist, DbClientPtr& client, int& outId) {
    try {
        auto result = client->execSqlSync(
            "INSERT INTO playlists (name, description) VALUES (?, ?)",
            playlist.name_, playlist.description_
        );
        outId = result.insertId();
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error creating playlist: " << e.base().what();
        return false;
    }
}

bool Playlist::update(const Playlist& playlist, DbClientPtr& client) {
    try {
        client->execSqlSync(
            "UPDATE playlists SET name = ?, description = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
            playlist.name_, playlist.description_, playlist.id_
        );
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error updating playlist: " << e.base().what();
        return false;
    }
}

bool Playlist::deleteById(int id, DbClientPtr& client) {
    try {
        client->execSqlSync("DELETE FROM playlists WHERE id = ?", id);
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error deleting playlist: " << e.base().what();
        return false;
    }
}

std::vector<Playlist> Playlist::findAll(DbClientPtr& client) {
    std::vector<Playlist> playlists;
    try {
        auto result = client->execSqlSync("SELECT * FROM playlists ORDER BY created_at DESC");
        
        for (const auto& row : result) {
            Playlist playlist;
            playlist.setId(row["id"].as<int>());
            playlist.setName(row["name"].as<std::string>());
            playlist.setDescription(row["description"].as<std::string>());
            playlist.setCreatedAt(row["created_at"].as<std::string>());
            playlist.setUpdatedAt(row["updated_at"].as<std::string>());
            playlists.push_back(playlist);
        }
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error finding playlists: " << e.base().what();
    }
    return playlists;
}

Playlist Playlist::findById(int id, DbClientPtr& client) {
    Playlist playlist;
    try {
        auto result = client->execSqlSync("SELECT * FROM playlists WHERE id = ?", id);
        
        if (result.size() > 0) {
            auto row = result[0];
            playlist.setId(row["id"].as<int>());
            playlist.setName(row["name"].as<std::string>());
            playlist.setDescription(row["description"].as<std::string>());
            playlist.setCreatedAt(row["created_at"].as<std::string>());
            playlist.setUpdatedAt(row["updated_at"].as<std::string>());
        }
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error finding playlist: " << e.base().what();
    }
    return playlist;
}

bool Playlist::addSongToPlaylist(int playlistId, int songId, int position, DbClientPtr& client) {
    try {
        client->execSqlSync(
            "INSERT INTO playlist_songs (playlist_id, song_id, position) VALUES (?, ?, ?)",
            playlistId, songId, position
        );
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error adding song to playlist: " << e.base().what();
        return false;
    }
}

bool Playlist::removeSongFromPlaylist(int playlistId, int songId, DbClientPtr& client) {
    try {
        client->execSqlSync(
            "DELETE FROM playlist_songs WHERE playlist_id = ? AND song_id = ?",
            playlistId, songId
        );
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error removing song from playlist: " << e.base().what();
        return false;
    }
}

bool Playlist::updateSongPositions(int playlistId, const std::vector<int>& songIds, 
                                    DbClientPtr& client) {
    try {
        // Start transaction
        client->execSqlSync("BEGIN TRANSACTION");
        
        for (size_t i = 0; i < songIds.size(); ++i) {
            client->execSqlSync(
                "UPDATE playlist_songs SET position = ? WHERE playlist_id = ? AND song_id = ?",
                static_cast<int>(i), playlistId, songIds[i]
            );
        }
        
        client->execSqlSync("COMMIT");
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error updating song positions: " << e.base().what();
        client->execSqlSync("ROLLBACK");
        return false;
    }
}

Json::Value Playlist::toJson() const {
    Json::Value json;
    json["id"] = id_;
    json["name"] = name_;
    json["description"] = description_;
    json["createdAt"] = createdAt_;
    json["updatedAt"] = updatedAt_;
    return json;
}

Json::Value Playlist::toJsonWithSongs(DbClientPtr& client) const {
    Json::Value json = toJson();
    
    auto songs = Song::findByPlaylistId(id_, client);
    Json::Value songsArray(Json::arrayValue);
    
    for (const auto& song : songs) {
        songsArray.append(song.toJson());
    }
    
    json["songs"] = songsArray;
    return json;
}