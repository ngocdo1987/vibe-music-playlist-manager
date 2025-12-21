#pragma once
#include <drogon/HttpController.h>

namespace controllers {

class AuthController : public drogon::HttpController<AuthController> {
public:
    METHOD_LIST_BEGIN
    ADD_METHOD_TO(AuthController::loginPage, "/admin/login", drogon::Get);
    ADD_METHOD_TO(AuthController::login, "/admin/login", drogon::Post);
    ADD_METHOD_TO(AuthController::logout, "/admin/logout", drogon::Get);
    METHOD_LIST_END
    
    void loginPage(const drogon::HttpRequestPtr& req,
                   std::function<void(const drogon::HttpResponsePtr&)>&& callback);
    
    void login(const drogon::HttpRequestPtr& req,
               std::function<void(const drogon::HttpResponsePtr&)>&& callback);
    
    void logout(const drogon::HttpRequestPtr& req,
                std::function<void(const drogon::HttpResponsePtr&)>&& callback);
    
private:
    std::string getEnvValue(const std::string& key);
};

} // namespace controllers