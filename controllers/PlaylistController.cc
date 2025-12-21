#include "controllers/PlaylistController.h"
#include "models/Playlist.h"
#include "models/Song.h"
#include <drogon/orm/DbClient.h>
#include <filesystem>
#include <fstream>
#include <chrono>
#include <random>

using namespace controllers;
using namespace drogon;
using namespace drogon::orm;
namespace fs = std::filesystem;

std::string PlaylistController::generateUniqueFilename(const std::string& originalName) {
    auto now = std::chrono::system_clock::now();
    auto timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()
    ).count();
    
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1000, 9999);
    int random = dis(gen);
    
    std::string extension = ".mp3";
    size_t dotPos = originalName.find_last_of('.');
    if (dotPos != std::string::npos) {
        extension = originalName.substr(dotPos);
    }
    
    return std::to_string(timestamp) + "_" + std::to_string(random) + extension;
}

bool PlaylistController::validateMP3File(const std::string& filePath) {
    std::ifstream file(filePath, std::ios::binary);
    if (!file.is_open()) return false;
    
    // Read first 3 bytes to check for MP3 signature
    unsigned char header[3];
    file.read(reinterpret_cast<char*>(header), 3);
    
    // Check for ID3 tag (ID3v2)
    if (header[0] == 'I' && header[1] == 'D' && header[2] == '3') {
        return true;
    }
    
    // Check for MP3 frame sync (0xFF 0xFB or 0xFF 0xFA)
    if (header[0] == 0xFF && (header[1] == 0xFB || header[1] == 0xFA)) {
        return true;
    }
    
    return false;
}

void PlaylistController::dashboard(const HttpRequestPtr& req,
                                    std::function<void(const HttpResponsePtr&)>&& callback) {
    auto dbClient = app().getDbClient();
    auto playlists = models::Playlist::findAll(dbClient);
    
    HttpViewData data;
    Json::Value playlistsJson(Json::arrayValue);
    
    for (const auto& playlist : playlists) {
        playlistsJson.append(playlist.toJson());
    }
    
    data.insert("playlists", playlistsJson.toStyledString());
    
    auto resp = HttpResponse::newHttpViewResponse("admin_dashboard", data);
    callback(resp);
}

void PlaylistController::createPage(const HttpRequestPtr& req,
                                     std::function<void(const HttpResponsePtr&)>&& callback) {
    HttpViewData data;
    data.insert("mode", "create");
    data.insert("playlist", "{}");
    data.insert("songs", "[]");
    
    auto resp = HttpResponse::newHttpViewResponse("playlist_form", data);
    callback(resp);
}

void PlaylistController::editPage(const HttpRequestPtr& req,
                                   std::function<void(const HttpResponsePtr&)>&& callback,
                                   int playlistId) {
    auto dbClient = app().getDbClient();
    auto playlist = models::Playlist::findById(playlistId, dbClient);
    
    if (playlist.getId() == 0) {
        auto resp = HttpResponse::newNotFoundResponse();
        callback(resp);
        return;
    }
    
    auto songs = models::Song::findByPlaylistId(playlistId, dbClient);
    
    HttpViewData data;
    data.insert("mode", "edit");
    data.insert("playlist", playlist.toJson().toStyledString());
    
    Json::Value songsJson(Json::arrayValue);
    for (const auto& song : songs) {
        songsJson.append(song.toJson());
    }
    data.insert("songs", songsJson.toStyledString());
    
    auto resp = HttpResponse::newHttpViewResponse("playlist_form", data);
    callback(resp);
}

void PlaylistController::create(const HttpRequestPtr& req,
                                 std::function<void(const HttpResponsePtr&)>&& callback) {
    auto name = req->getParameter("name");
    auto description = req->getParameter("description");
    auto songIdsStr = req->getParameter("song_ids");
    
    if (name.empty()) {
        Json::Value json;
        json["error"] = "Playlist name is required";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    auto dbClient = app().getDbClient();
    models::Playlist playlist;
    playlist.setName(name);
    playlist.setDescription(description);
    
    int playlistId;
    if (models::Playlist::create(playlist, dbClient, playlistId)) {
        // Add songs to playlist if any
        if (!songIdsStr.empty()) {
            Json::Value songIds;
            Json::Reader reader;
            if (reader.parse(songIdsStr, songIds) && songIds.isArray()) {
                for (size_t i = 0; i < songIds.size(); ++i) {
                    int songId = songIds[static_cast<int>(i)].asInt();
                    models::Playlist::addSongToPlaylist(playlistId, songId, static_cast<int>(i), dbClient);
                }
            }
        }
        
        auto resp = HttpResponse::newRedirectionResponse("/admin/dashboard");
        callback(resp);
    } else {
        Json::Value json;
        json["error"] = "Failed to create playlist";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k500InternalServerError);
        callback(resp);
    }
}

void PlaylistController::update(const HttpRequestPtr& req,
                                 std::function<void(const HttpResponsePtr&)>&& callback,
                                 int playlistId) {
    auto name = req->getParameter("name");
    auto description = req->getParameter("description");
    auto songIdsStr = req->getParameter("song_ids");
    
    if (name.empty()) {
        Json::Value json;
        json["error"] = "Playlist name is required";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    auto dbClient = app().getDbClient();
    models::Playlist playlist;
    playlist.setId(playlistId);
    playlist.setName(name);
    playlist.setDescription(description);
    
    if (models::Playlist::update(playlist, dbClient)) {
        // Update song positions
        if (!songIdsStr.empty()) {
            Json::Value songIdsJson;
            Json::Reader reader;
            if (reader.parse(songIdsStr, songIdsJson) && songIdsJson.isArray()) {
                std::vector<int> songIds;
                for (const auto& id : songIdsJson) {
                    songIds.push_back(id.asInt());
                }
                models::Playlist::updateSongPositions(playlistId, songIds, dbClient);
            }
        }
        
        auto resp = HttpResponse::newRedirectionResponse("/admin/dashboard");
        callback(resp);
    } else {
        Json::Value json;
        json["error"] = "Failed to update playlist";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k500InternalServerError);
        callback(resp);
    }
}

void PlaylistController::deletePlaylist(const HttpRequestPtr& req,
                                         std::function<void(const HttpResponsePtr&)>&& callback,
                                         int playlistId) {
    auto dbClient = app().getDbClient();
    
    if (models::Playlist::deleteById(playlistId, dbClient)) {
        auto resp = HttpResponse::newRedirectionResponse("/admin/dashboard");
        callback(resp);
    } else {
        Json::Value json;
        json["error"] = "Failed to delete playlist";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k500InternalServerError);
        callback(resp);
    }
}

void PlaylistController::uploadSong(const HttpRequestPtr& req,
                                     std::function<void(const HttpResponsePtr&)>&& callback) {
    MultiPartParser fileUpload;
    if (fileUpload.parse(req) != 0) {
        Json::Value json;
        json["error"] = "Failed to parse upload";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    auto files = fileUpload.getFiles();
    if (files.empty()) {
        Json::Value json;
        json["error"] = "No file uploaded";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    auto file = files[0];
    
    // Check file extension
    std::string originalName = file.getFileName();
    if (originalName.size() < 4 || 
        originalName.substr(originalName.size() - 4) != ".mp3") {
        Json::Value json;
        json["error"] = "Only MP3 files are allowed";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    // Generate unique filename
    std::string uniqueFilename = generateUniqueFilename(originalName);
    std::string filePath = "./mp3/" + uniqueFilename;
    
    // Save file
    file.saveAs(filePath);
    
    // Validate MP3 file
    if (!validateMP3File(filePath)) {
        fs::remove(filePath);
        Json::Value json;
        json["error"] = "Invalid MP3 file format";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    // Save to database
    auto dbClient = app().getDbClient();
    models::Song song;
    song.setFilename(uniqueFilename);
    song.setOriginalName(originalName);
    song.setFilePath(filePath);
    song.setFileSize(static_cast<int>(file.fileLength()));
    
    int songId;
    if (models::Song::create(song, dbClient, songId)) {
        song.setId(songId);
        Json::Value json = song.toJson();
        json["success"] = true;
        
        auto resp = HttpResponse::newHttpJsonResponse(json);
        callback(resp);
    } else {
        fs::remove(filePath);
        Json::Value json;
        json["error"] = "Failed to save song to database";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k500InternalServerError);
        callback(resp);
    }
}

void PlaylistController::deleteSong(const HttpRequestPtr& req,
                                     std::function<void(const HttpResponsePtr&)>&& callback,
                                     int songId) {
    auto dbClient = app().getDbClient();
    auto song = models::Song::findById(songId, dbClient);
    
    if (song.getId() == 0) {
        Json::Value json;
        json["error"] = "Song not found";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k404NotFound);
        callback(resp);
        return;
    }
    
    // Delete file from disk
    try {
        fs::remove(song.getFilePath());
    } catch (...) {
        LOG_WARN << "Failed to delete file: " << song.getFilePath();
    }
    
    // Delete from database
    if (models::Song::deleteById(songId, dbClient)) {
        Json::Value json;
        json["success"] = true;
        auto resp = HttpResponse::newHttpJsonResponse(json);
        callback(resp);
    } else {
        Json::Value json;
        json["error"] = "Failed to delete song";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k500InternalServerError);
        callback(resp);
    }
}

void PlaylistController::updatePositions(const HttpRequestPtr& req,
                                          std::function<void(const HttpResponsePtr&)>&& callback,
                                          int playlistId) {
    auto jsonPtr = req->getJsonObject();
    if (!jsonPtr) {
        Json::Value json;
        json["error"] = "Invalid JSON";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    auto songIdsJson = (*jsonPtr)["song_ids"];
    if (!songIdsJson.isArray()) {
        Json::Value json;
        json["error"] = "song_ids must be an array";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k400BadRequest);
        callback(resp);
        return;
    }
    
    std::vector<int> songIds;
    for (const auto& id : songIdsJson) {
        songIds.push_back(id.asInt());
    }
    
    auto dbClient = app().getDbClient();
    if (models::Playlist::updateSongPositions(playlistId, songIds, dbClient)) {
        Json::Value json;
        json["success"] = true;
        auto resp = HttpResponse::newHttpJsonResponse(json);
        callback(resp);
    } else {
        Json::Value json;
        json["error"] = "Failed to update positions";
        auto resp = HttpResponse::newHttpJsonResponse(json);
        resp->setStatusCode(k500InternalServerError);
        callback(resp);
    }
}

void PlaylistController::player(const HttpRequestPtr& req,
                                 std::function<void(const HttpResponsePtr&)>&& callback,
                                 int playlistId) {
    auto dbClient = app().getDbClient();
    auto playlist = models::Playlist::findById(playlistId, dbClient);
    
    if (playlist.getId() == 0) {
        auto resp = HttpResponse::newNotFoundResponse();
        callback(resp);
        return;
    }
    
    HttpViewData data;
    data.insert("playlist", playlist.toJsonWithSongs(dbClient).toStyledString());
    
    auto resp = HttpResponse::newHttpViewResponse("player", data);
    callback(resp);
}

void PlaylistController::serveSong(const HttpRequestPtr& req,
                                    std::function<void(const HttpResponsePtr&)>&& callback,
                                    const std::string& filename) {
    std::string filePath = "./mp3/" + filename;
    
    if (!fs::exists(filePath)) {
        auto resp = HttpResponse::newNotFoundResponse();
        callback(resp);
        return;
    }
    
    auto resp = HttpResponse::newFileResponse(filePath, "", CT_CUSTOM);
    resp->setContentTypeString("audio/mpeg");
    callback(resp);
}