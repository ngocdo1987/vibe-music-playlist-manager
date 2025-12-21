#pragma once
#include <drogon/HttpController.h>
#include <string>
#include <vector>

namespace models {

class Song {
public:
    Song() = default;
    Song(int id, const std::string& filename, const std::string& originalName, 
         const std::string& filePath, int fileSize);
    
    // Getters
    int getId() const { return id_; }
    std::string getFilename() const { return filename_; }
    std::string getOriginalName() const { return originalName_; }
    std::string getFilePath() const { return filePath_; }
    int getFileSize() const { return fileSize_; }
    std::string getCreatedAt() const { return createdAt_; }
    
    // Setters
    void setId(int id) { id_ = id; }
    void setFilename(const std::string& filename) { filename_ = filename; }
    void setOriginalName(const std::string& name) { originalName_ = name; }
    void setFilePath(const std::string& path) { filePath_ = path; }
    void setFileSize(int size) { fileSize_ = size; }
    void setCreatedAt(const std::string& time) { createdAt_ = time; }
    
    // Database operations
    static bool create(const Song& song, drogon::orm::DbClientPtr& client, int& outId);
    static std::vector<Song> findByPlaylistId(int playlistId, drogon::orm::DbClientPtr& client);
    static Song findById(int id, drogon::orm::DbClientPtr& client);
    static bool deleteById(int id, drogon::orm::DbClientPtr& client);
    
    Json::Value toJson() const;
    
private:
    int id_;
    std::string filename_;
    std::string originalName_;
    std::string filePath_;
    int fileSize_;
    std::string createdAt_;
};

} // namespace models