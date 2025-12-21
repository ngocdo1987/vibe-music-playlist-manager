#include <drogon/drogon.h>
#include <iostream>
#include <fstream>
#include <filesystem>

using namespace drogon;
namespace fs = std::filesystem;

void initializeDatabase() {
    // Get the default database client
    auto dbClient = app().getDbClient();
    
    try {
        // Read and execute SQL schema
        std::ifstream sqlFile("database.sql");
        if (!sqlFile.is_open()) {
            LOG_ERROR << "Failed to open database.sql";
            return;
        }
        
        std::string sql((std::istreambuf_iterator<char>(sqlFile)),
                        std::istreambuf_iterator<char>());
        
        // Split SQL statements by semicolon and execute each
        size_t pos = 0;
        std::string delimiter = ";";
        while ((pos = sql.find(delimiter)) != std::string::npos) {
            std::string statement = sql.substr(0, pos);
            
            // Trim whitespace
            statement.erase(0, statement.find_first_not_of(" \n\r\t"));
            statement.erase(statement.find_last_not_of(" \n\r\t") + 1);
            
            if (!statement.empty()) {
                try {
                    dbClient->execSqlSync(statement);
                } catch (const drogon::orm::DrogonDbException& e) {
                    LOG_WARN << "SQL execution warning: " << e.base().what();
                }
            }
            
            sql.erase(0, pos + delimiter.length());
        }
        
        LOG_INFO << "Database initialized successfully";
    } catch (const std::exception& e) {
        LOG_ERROR << "Error initializing database: " << e.what();
    }
}

void createDirectories() {
    // Create necessary directories if they don't exist
    std::vector<std::string> dirs = {"mp3", "logs", "public/css", "public/js", "views"};
    
    for (const auto& dir : dirs) {
        if (!fs::exists(dir)) {
            fs::create_directories(dir);
            LOG_INFO << "Created directory: " << dir;
        }
    }
}

void setupRoutes() {
    // Root route - redirect to admin login
    app().registerHandler(
        "/",
        [](const HttpRequestPtr& req,
           std::function<void(const HttpResponsePtr&)>&& callback) {
            auto resp = HttpResponse::newRedirectionResponse("/admin/login");
            callback(resp);
        },
        {Get}
    );
    
    LOG_INFO << "Routes configured successfully";
}

int main() {
    try {
        // Create necessary directories first
        createDirectories();
        
        // Load configuration file
        app().loadConfigFile("./config.json");
        
        // Setup routes
        setupRoutes();
        
        // Set up callback to initialize database after Drogon starts
        app().registerBeginningAdvice([]() {
            LOG_INFO << "Initializing database...";
            initializeDatabase();
            LOG_INFO << "Application ready!";
            LOG_INFO << "Access the application at:";
            LOG_INFO << "  - Admin Panel: http://0.0.0.0:3534/admin/login";
            LOG_INFO << "  - Root URL: http://0.0.0.0:3534/";
        });
        
        LOG_INFO << "Starting Music Playlist Manager...";
        LOG_INFO << "Loading controllers...";
        
        // Run the application
        app().run();
    } catch (const std::exception& e) {
        LOG_ERROR << "Fatal error: " << e.what();
        return 1;
    }
    
    return 0;
}