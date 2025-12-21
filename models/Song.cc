#include "models/Song.h"
#include <drogon/orm/DbClient.h>

using namespace models;
using namespace drogon::orm;

Song::Song(int id, const std::string& filename, const std::string& originalName, 
           const std::string& filePath, int fileSize)
    : id_(id), filename_(filename), originalName_(originalName), 
      filePath_(filePath), fileSize_(fileSize) {}

bool Song::create(const Song& song, DbClientPtr& client, int& outId) {
    try {
        auto result = client->execSqlSync(
            "INSERT INTO songs (filename, original_name, file_path, file_size) VALUES (?, ?, ?, ?)",
            song.filename_, song.originalName_, song.filePath_, song.fileSize_
        );
        outId = result.insertId();
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error creating song: " << e.base().what();
        return false;
    }
}

std::vector<Song> Song::findByPlaylistId(int playlistId, DbClientPtr& client) {
    std::vector<Song> songs;
    try {
        auto result = client->execSqlSync(
            "SELECT s.* FROM songs s "
            "INNER JOIN playlist_songs ps ON s.id = ps.song_id "
            "WHERE ps.playlist_id = ? ORDER BY ps.position",
            playlistId
        );
        
        for (const auto& row : result) {
            Song song;
            song.setId(row["id"].as<int>());
            song.setFilename(row["filename"].as<std::string>());
            song.setOriginalName(row["original_name"].as<std::string>());
            song.setFilePath(row["file_path"].as<std::string>());
            song.setFileSize(row["file_size"].as<int>());
            song.setCreatedAt(row["created_at"].as<std::string>());
            songs.push_back(song);
        }
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error finding songs: " << e.base().what();
    }
    return songs;
}

Song Song::findById(int id, DbClientPtr& client) {
    Song song;
    try {
        auto result = client->execSqlSync(
            "SELECT * FROM songs WHERE id = ?", id
        );
        
        if (result.size() > 0) {
            auto row = result[0];
            song.setId(row["id"].as<int>());
            song.setFilename(row["filename"].as<std::string>());
            song.setOriginalName(row["original_name"].as<std::string>());
            song.setFilePath(row["file_path"].as<std::string>());
            song.setFileSize(row["file_size"].as<int>());
            song.setCreatedAt(row["created_at"].as<std::string>());
        }
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error finding song: " << e.base().what();
    }
    return song;
}

bool Song::deleteById(int id, DbClientPtr& client) {
    try {
        client->execSqlSync("DELETE FROM songs WHERE id = ?", id);
        return true;
    } catch (const DrogonDbException& e) {
        LOG_ERROR << "Error deleting song: " << e.base().what();
        return false;
    }
}

Json::Value Song::toJson() const {
    Json::Value json;
    json["id"] = id_;
    json["filename"] = filename_;
    json["originalName"] = originalName_;
    json["filePath"] = filePath_;
    json["fileSize"] = fileSize_;
    json["createdAt"] = createdAt_;
    return json;
}