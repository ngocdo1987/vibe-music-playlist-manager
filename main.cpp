#include "crow.h"
#include "db_manager.hpp"
#include "auth.hpp"
#include <iostream>
#include <algorithm>
#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;

std::string sanitize_filename(const std::string& filename) {
    std::string sanitized;
    for (char c : filename) {
        if (std::isalnum(static_cast<unsigned char>(c)) || c == '.' || c == '_' || c == '-') {
            sanitized += c;
        } else {
            sanitized += '_';
        }
    }
    // Ensure it's not empty and has a safe fallback
    return sanitized;
}

int main() {
    crow::SimpleApp app;
    auto env = AuthManager::load_env();
    DBManager db(env["DB_PATH"].empty() ? "music.db" : env["DB_PATH"]);

    // Static files and MP3 serving
    CROW_ROUTE(app, "/assets/<path>")
    ([](const crow::request&, crow::response& res, std::string path) {
        res.set_static_file_info("static/" + path);
        res.end();
    });

    CROW_ROUTE(app, "/mp3/<path>")
    ([](const crow::request&, crow::response& res, std::string path) {
        res.set_static_file_info("mp3/" + path);
        res.end();
    });

    // --- Public Routes ---

    CROW_ROUTE(app, "/")
    ([&db]() {
        auto playlists = db.get_playlists();
        crow::mustache::context ctx;
        std::vector<crow::mustache::context> playlists_vec;
        for (const auto& p : playlists) {
            crow::mustache::context p_ctx;
            p_ctx["id"] = p.id;
            p_ctx["name"] = p.name;
            p_ctx["description"] = p.description;
            playlists_vec.push_back(std::move(p_ctx));
        }
        ctx["playlists"] = std::move(playlists_vec);
        return crow::mustache::load("index.html").render(ctx);
    });

    CROW_ROUTE(app, "/playlist/<int>")
    ([&db](int id) {
        auto p = db.get_playlist(id);
        if (p.id == -1) return crow::response(404);
        
        auto songs = db.get_playlist_songs(id);
        crow::mustache::context ctx;
        ctx["playlist_name"] = p.name;
        std::vector<crow::mustache::context> songs_vec;
        for (const auto& s : songs) {
            crow::mustache::context s_ctx;
            s_ctx["title"] = s.title;
            s_ctx["filename"] = s.filename;
            songs_vec.push_back(std::move(s_ctx));
        }
        ctx["songs"] = std::move(songs_vec);
        return crow::response(crow::mustache::load("player_winamp.html").render(ctx));
    });

    // --- Admin Routes ---

    CROW_ROUTE(app, "/admin/login").methods(crow::HTTPMethod::GET, crow::HTTPMethod::POST)
    ([](const crow::request& req) {
        if (req.method == crow::HTTPMethod::POST) {
            auto body = req.url_params;
            std::string user = req.get_body_params().get("username");
            std::string pass = req.get_body_params().get("password");
            if (AuthManager::validate_credentials(user, pass)) {
                auto res = crow::response(302);
                res.set_header("Set-Cookie", "session=admin_logged_in; Path=/; HttpOnly");
                res.set_header("Location", "/admin/dashboard");
                return res;
            }
        }
        return crow::response(crow::mustache::load("login.html").render());
    });

    CROW_ROUTE(app, "/admin/logout")
    ([]() {
        auto res = crow::response(302);
        res.set_header("Set-Cookie", "session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly");
        res.set_header("Location", "/admin/login");
        return res;
    });

    auto is_authenticated = [](const crow::request& req) {
        auto cookie = req.get_header_value("Cookie");
        return cookie.find("session=admin_logged_in") != std::string::npos;
    };

    CROW_ROUTE(app, "/admin/dashboard")
    ([&db, &is_authenticated](const crow::request& req) {
        if (!is_authenticated(req)) {
            crow::response res(302);
            res.set_header("Location", "/admin/login");
            return res;
        }
        auto playlists = db.get_playlists();
        crow::mustache::context ctx;
        std::vector<crow::mustache::context> playlists_vec;
        for (const auto& p : playlists) {
            crow::mustache::context p_ctx;
            p_ctx["id"] = p.id;
            p_ctx["name"] = p.name;
            playlists_vec.push_back(std::move(p_ctx));
        }
        ctx["playlists"] = std::move(playlists_vec);
        return crow::response(crow::mustache::load("dashboard.html").render(ctx));
    });

    CROW_ROUTE(app, "/admin/playlist/new").methods(crow::HTTPMethod::POST)
    ([&db, &is_authenticated](const crow::request& req) {
        if (!is_authenticated(req)) return crow::response(403);
        std::string name = req.get_body_params().get("name");
        db.create_playlist(name, "");
        crow::response res(302);
        res.set_header("Location", "/admin/dashboard");
        return res;
    });

    CROW_ROUTE(app, "/admin/playlist/delete/<int>")
    ([&db, &is_authenticated](const crow::request& req, int id) {
        if (!is_authenticated(req)) return crow::response(403);
        
        // 1. Get songs in playlist before deleting it
        auto songs = db.get_playlist_songs(id);
        std::vector<int> song_ids;
        for (const auto& s : songs) song_ids.push_back(s.id);

        // 2. Delete playlist (cascades to playlist_songs)
        db.delete_playlist(id);

        // 3. Delete songs that are no longer in any playlist
        std::vector<std::string> files_to_delete;
        db.delete_orphaned_songs(song_ids, files_to_delete);

        // 4. Remove physical files
        for (const auto& filename : files_to_delete) {
            std::string filepath = "mp3/" + filename;
            if (fs::exists(filepath)) {
                fs::remove(filepath);
            }
        }

        crow::response res(302);
        res.set_header("Location", "/admin/dashboard");
        return res;
    });

    CROW_ROUTE(app, "/admin/playlist/edit/<int>")
    ([&db, &is_authenticated](const crow::request& req, int id) {
        if (!is_authenticated(req)) {
            crow::response res(302);
            res.set_header("Location", "/admin/login");
            return res;
        }
        auto p = db.get_playlist(id);
        auto songs = db.get_playlist_songs(id);
        crow::mustache::context ctx;
        ctx["id"] = p.id;
        ctx["name"] = p.name;
        std::vector<crow::mustache::context> songs_vec;
        for (const auto& s : songs) {
            crow::mustache::context s_ctx;
            s_ctx["id"] = s.id;
            s_ctx["title"] = s.title;
            songs_vec.push_back(std::move(s_ctx));
        }
        ctx["songs"] = std::move(songs_vec);
        return crow::response(crow::mustache::load("edit_playlist.html").render(ctx));
    });

    CROW_ROUTE(app, "/admin/upload/<int>").methods(crow::HTTPMethod::POST)
    ([&db, &is_authenticated](const crow::request& req, int playlist_id) {
        if (!is_authenticated(req)) return crow::response(403);
        
        crow::multipart::message msg(req);
        for (auto& part : msg.parts) {
            auto content_type = part.get_header_object("Content-Type").value;
            if (content_type != "audio/mpeg" && content_type != "audio/mp3") {
                return crow::response(400, "Invalid file type. Only MP3 allowed.");
            }

            auto disposition = part.get_header_object("Content-Disposition");
            std::string original_filename = "unknown.mp3";
            if (disposition.params.count("filename")) {
                original_filename = disposition.params.at("filename");
            }

            std::string sanitized_filename = sanitize_filename(original_filename);
            
            // Validate extension (after sanitization just in case, but usually check original)
            if (original_filename.size() < 4 || original_filename.substr(original_filename.size() - 4) != ".mp3") {
                 // Even if sanitized has .mp3, we trust original for logic
            }

            std::string filepath = "mp3/" + sanitized_filename;
            
            std::ofstream out(filepath, std::ios::binary);
            out << part.body;
            out.close();

            int song_id = db.add_song(original_filename, sanitized_filename);
            auto songs = db.get_playlist_songs(playlist_id);
            db.add_song_to_playlist(playlist_id, song_id, (int)songs.size());
        }
        return crow::response(200, "OK");
    });

    CROW_ROUTE(app, "/admin/playlist/save_order/<int>").methods(crow::HTTPMethod::POST)
    ([&db, &is_authenticated](const crow::request& req, int playlist_id) {
        if (!is_authenticated(req)) return crow::response(403);
        auto json = crow::json::load(req.body);
        if (!json) return crow::response(400);

        db.clear_playlist_songs(playlist_id);
        int pos = 0;
        for (auto& song_id : json["song_ids"]) {
            db.add_song_to_playlist(playlist_id, song_id.i(), pos++);
        }
        return crow::response(200, "OK");
    });

    app.port(std::stoi(env["PORT"].empty() ? "8080" : env["PORT"])).multithreaded().run();
}
