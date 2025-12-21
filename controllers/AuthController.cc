#include "controllers/AuthController.h"
#include <fstream>
#include <sstream>

using namespace controllers;
using namespace drogon;

std::string AuthController::getEnvValue(const std::string& key) {
    std::ifstream file(".env");
    std::string line;
    
    while (std::getline(file, line)) {
        // Skip empty lines and comments
        if (line.empty() || line[0] == '#') continue;
        
        size_t pos = line.find('=');
        if (pos != std::string::npos) {
            std::string envKey = line.substr(0, pos);
            std::string envValue = line.substr(pos + 1);
            
            // Trim whitespace
            envKey.erase(0, envKey.find_first_not_of(" \t"));
            envKey.erase(envKey.find_last_not_of(" \t") + 1);
            envValue.erase(0, envValue.find_first_not_of(" \t"));
            envValue.erase(envValue.find_last_not_of(" \t") + 1);
            
            if (envKey == key) {
                return envValue;
            }
        }
    }
    return "";
}

void AuthController::loginPage(const HttpRequestPtr& req,
                                std::function<void(const HttpResponsePtr&)>&& callback) {
    auto session = req->session();
    if (session && session->find("authenticated")) {
        auto resp = HttpResponse::newRedirectionResponse("/admin/dashboard");
        callback(resp);
        return;
    }
    
    HttpViewData data;
    data.insert("error", req->getParameter("error"));
    
    auto resp = HttpResponse::newHttpViewResponse("admin_login", data);
    callback(resp);
}

void AuthController::login(const HttpRequestPtr& req,
                           std::function<void(const HttpResponsePtr&)>&& callback) {
    auto username = req->getParameter("username");
    auto password = req->getParameter("password");
    
    std::string adminUser = getEnvValue("ADMIN_USERNAME");
    std::string adminPass = getEnvValue("ADMIN_PASSWORD");
    
    if (username == adminUser && password == adminPass) {
        auto session = req->session();
        session->insert("authenticated", true);
        session->insert("username", username);
        
        auto resp = HttpResponse::newRedirectionResponse("/admin/dashboard");
        callback(resp);
    } else {
        auto resp = HttpResponse::newRedirectionResponse("/admin/login?error=1");
        callback(resp);
    }
}

void AuthController::logout(const HttpRequestPtr& req,
                            std::function<void(const HttpResponsePtr&)>&& callback) {
    auto session = req->session();
    session->clear();
    
    auto resp = HttpResponse::newRedirectionResponse("/admin/login");
    callback(resp);
}