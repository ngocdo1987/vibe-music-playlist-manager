#pragma once
#include <string>
#include <fstream>
#include <sstream>
#include <map>

class AuthManager {
public:
    static std::map<std::string, std::string> load_env(const std::string& filename = ".env") {
        std::map<std::string, std::string> env;
        std::ifstream file(filename);
        std::string line;
        while (std::getline(file, line)) {
            size_t pos = line.find('=');
            if (pos != std::string::npos) {
                std::string key = line.substr(0, pos);
                std::string value = line.substr(pos + 1);
                env[key] = value;
            }
        }
        return env;
    }

    static bool validate_credentials(const std::string& user, const std::string& pass) {
        auto env = load_env();
        return env["ADMIN_USER"] == user && env["ADMIN_PASS"] == pass;
    }
};
