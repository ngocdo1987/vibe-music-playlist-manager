#pragma once
#include <drogon/HttpController.h>
#include "models/Song.h"
#include <string>
#include <vector>

namespace models {

class Playlist {
public:
    Playlist() = default;
    Playlist(int id, const std::string& name, const std::string& description);
    
    // Getters
    int getId() const { return id_; }
    std::string getName() const { return name_; }
    std::string getDescription() const { return description_; }
    std::string getCreatedAt() const { return createdAt_; }
    std::string getUpdatedAt() const { return updatedAt_; }
    
    // Setters
    void setId(int id) { id_ = id; }
    void setName(const std::string& name) { name_ = name; }
    void setDescription(const std::string& desc) { description_ = desc; }
    void setCreatedAt(const std::string& time) { createdAt_ = time; }
    void setUpdatedAt(const std::string& time) { updatedAt_ = time; }
    
    // Database operations
    static bool create(const Playlist& playlist, drogon::orm::DbClientPtr& client, int& outId);
    static bool update(const Playlist& playlist, drogon::orm::DbClientPtr& client);
    static bool deleteById(int id, drogon::orm::DbClientPtr& client);
    static std::vector<Playlist> findAll(drogon::orm::DbClientPtr& client);
    static Playlist findById(int id, drogon::orm::DbClientPtr& client);
    
    // Playlist-Song operations
    static bool addSongToPlaylist(int playlistId, int songId, int position, 
                                   drogon::orm::DbClientPtr& client);
    static bool removeSongFromPlaylist(int playlistId, int songId, 
                                        drogon::orm::DbClientPtr& client);
    static bool updateSongPositions(int playlistId, const std::vector<int>& songIds, 
                                     drogon::orm::DbClientPtr& client);
    
    Json::Value toJson() const;
    Json::Value toJsonWithSongs(drogon::orm::DbClientPtr& client) const;
    
private:
    int id_;
    std::string name_;
    std::string description_;
    std::string createdAt_;
    std::string updatedAt_;
};

} // namespace models