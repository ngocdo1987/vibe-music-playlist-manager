#pragma once
#include <drogon/HttpController.h>

using namespace drogon;

namespace controllers {

class PlaylistController : public drogon::HttpController<PlaylistController> {
public:
    METHOD_LIST_BEGIN
    // Admin routes (require authentication)
    ADD_METHOD_TO(PlaylistController::dashboard, "/admin/dashboard", Get, "filters::AuthFilter");
    ADD_METHOD_TO(PlaylistController::createPage, "/admin/playlist/create", Get, "filters::AuthFilter");
    ADD_METHOD_TO(PlaylistController::editPage, "/admin/playlist/edit/{1}", Get, "filters::AuthFilter");
    ADD_METHOD_TO(PlaylistController::create, "/admin/playlist", Post, "filters::AuthFilter");
    ADD_METHOD_TO(PlaylistController::update, "/admin/playlist/{1}", Post, "filters::AuthFilter");
    ADD_METHOD_TO(PlaylistController::deletePlaylist, "/admin/playlist/delete/{1}", Post, "filters::AuthFilter");
    
    // API routes for AJAX (require authentication)
    ADD_METHOD_TO(PlaylistController::uploadSong, "/api/upload-song", Post, "filters::AuthFilter");
    ADD_METHOD_TO(PlaylistController::deleteSong, "/api/song/{1}", Delete, "filters::AuthFilter");
    ADD_METHOD_TO(PlaylistController::updatePositions, "/api/playlist/{1}/positions", Post, "filters::AuthFilter");
    
    // Public routes (no authentication required)
    ADD_METHOD_TO(PlaylistController::player, "/playlist/{1}", Get);
    ADD_METHOD_TO(PlaylistController::serveSong, "/mp3/{1}", Get);
    METHOD_LIST_END
    
    void dashboard(const HttpRequestPtr& req,
                   std::function<void(const HttpResponsePtr&)>&& callback);
    
    void createPage(const HttpRequestPtr& req,
                    std::function<void(const HttpResponsePtr&)>&& callback);
    
    void editPage(const HttpRequestPtr& req,
                  std::function<void(const HttpResponsePtr&)>&& callback,
                  int playlistId);
    
    void create(const HttpRequestPtr& req,
                std::function<void(const HttpResponsePtr&)>&& callback);
    
    void update(const HttpRequestPtr& req,
                std::function<void(const HttpResponsePtr&)>&& callback,
                int playlistId);
    
    void deletePlaylist(const HttpRequestPtr& req,
                        std::function<void(const HttpResponsePtr&)>&& callback,
                        int playlistId);
    
    void uploadSong(const HttpRequestPtr& req,
                    std::function<void(const HttpResponsePtr&)>&& callback);
    
    void deleteSong(const HttpRequestPtr& req,
                    std::function<void(const HttpResponsePtr&)>&& callback,
                    int songId);
    
    void updatePositions(const HttpRequestPtr& req,
                         std::function<void(const HttpResponsePtr&)>&& callback,
                         int playlistId);
    
    void player(const HttpRequestPtr& req,
                std::function<void(const HttpResponsePtr&)>&& callback,
                int playlistId);
    
    void serveSong(const HttpRequestPtr& req,
                   std::function<void(const HttpResponsePtr&)>&& callback,
                   const std::string& filename);
    
private:
    bool validateMP3File(const std::string& filePath);
    std::string generateUniqueFilename(const std::string& originalName);
};

} // namespace controllers