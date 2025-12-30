// Authentication module - Login validation and .env credentials
const std = @import("std");
const session = @import("utils/session.zig");

/// Authentication configuration loaded from .env
pub const AuthConfig = struct {
    admin_username: []const u8,
    admin_password: []const u8,
    server_port: u16,

    /// Load credentials from .env file
    pub fn loadFromEnv(allocator: std.mem.Allocator) !AuthConfig {
        const file = std.fs.cwd().openFile(".env", .{}) catch {
            // Default credentials if .env not found
            return AuthConfig{
                .admin_username = "admin",
                .admin_password = "admin123",
                .server_port = 8080,
            };
        };
        defer file.close();

        var username: []const u8 = "admin";
        var password: []const u8 = "admin123";
        var port: u16 = 8080;
        var found_username = false;
        var found_password = false;

        const content = try file.readToEndAlloc(allocator, 4096);
        defer allocator.free(content);

        var lines = std.mem.splitScalar(u8, content, '\n');
        while (lines.next()) |line| {
            // Skip comments and empty lines
            const trimmed = std.mem.trim(u8, line, " \t\r");
            if (trimmed.len == 0 or trimmed[0] == '#') continue;

            if (std.mem.indexOf(u8, trimmed, "=")) |eq_pos| {
                const key = std.mem.trim(u8, trimmed[0..eq_pos], " \t");
                const value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " \t");

                if (std.mem.eql(u8, key, "ADMIN_USERNAME")) {
                    username = try allocator.dupe(u8, value);
                    found_username = true;
                } else if (std.mem.eql(u8, key, "ADMIN_PASSWORD")) {
                    password = try allocator.dupe(u8, value);
                    found_password = true;
                } else if (std.mem.eql(u8, key, "SERVER_PORT")) {
                    port = std.fmt.parseInt(u16, value, 10) catch 8080;
                }
            }
        }

        // If not found in file, dupe the defaults
        if (!found_username) username = try allocator.dupe(u8, "admin");
        if (!found_password) password = try allocator.dupe(u8, "admin123");

        return AuthConfig{
            .admin_username = username,
            .admin_password = password,
            .server_port = port,
        };
    }
};

/// Authentication manager
pub const Auth = struct {
    config: AuthConfig,
    sessions: session.SessionManager,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{
            .config = try AuthConfig.loadFromEnv(allocator),
            .sessions = session.SessionManager.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.sessions.deinit();
    }

    /// Validate login credentials
    pub fn validateCredentials(self: *Self, username: []const u8, password: []const u8) bool {
        return std.mem.eql(u8, username, self.config.admin_username) and
            std.mem.eql(u8, password, self.config.admin_password);
    }

    /// Login user and create session
    pub fn login(self: *Self, username: []const u8, password: []const u8) !?[]const u8 {
        if (self.validateCredentials(username, password)) {
            return try self.sessions.createSession(username);
        }
        return null;
    }

    /// Check if request is authenticated
    pub fn isAuthenticated(self: *Self, cookie_header: ?[]const u8) bool {
        const session_id = session.getSessionIdFromCookies(cookie_header);
        return self.sessions.isAuthenticated(session_id);
    }

    /// Get session from cookie header
    pub fn getSessionFromCookies(self: *Self, cookie_header: ?[]const u8) ?*session.Session {
        const session_id = session.getSessionIdFromCookies(cookie_header) orelse return null;
        return self.sessions.getSession(session_id);
    }

    /// Logout user
    pub fn logout(self: *Self, cookie_header: ?[]const u8) void {
        if (session.getSessionIdFromCookies(cookie_header)) |session_id| {
            self.sessions.destroySession(session_id);
        }
    }
};

/// Parse form data (application/x-www-form-urlencoded)
pub fn parseFormData(allocator: std.mem.Allocator, body: []const u8) !std.StringHashMap([]const u8) {
    var result = std.StringHashMap([]const u8).init(allocator);
    errdefer result.deinit();

    var pairs = std.mem.splitScalar(u8, body, '&');
    while (pairs.next()) |pair| {
        if (std.mem.indexOf(u8, pair, "=")) |eq_pos| {
            const key = pair[0..eq_pos];
            const value = pair[eq_pos + 1 ..];
            // URL decode would go here - simplified for now
            try result.put(key, value);
        }
    }

    return result;
}

test "parseFormData" {
    const allocator = std.testing.allocator;
    var form = try parseFormData(allocator, "username=admin&password=secret");
    defer form.deinit();

    try std.testing.expectEqualStrings("admin", form.get("username").?);
    try std.testing.expectEqualStrings("secret", form.get("password").?);
}
