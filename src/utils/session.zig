// Session module - Cookie-based session management
const std = @import("std");

/// Session data
pub const Session = struct {
    authenticated: bool,
    username: []const u8,
    created_at: i64,
    last_access: i64,
};

/// Session manager - stores sessions in memory
pub const SessionManager = struct {
    sessions: std.StringHashMap(Session),
    allocator: std.mem.Allocator,

    const Self = @This();
    const SESSION_COOKIE_NAME = "session_id";
    const SESSION_MAX_AGE: i64 = 24 * 60 * 60; // 24 hours

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .sessions = std.StringHashMap(Session).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all session IDs
        var iter = self.sessions.keyIterator();
        while (iter.next()) |key| {
            self.allocator.free(key.*);
        }
        self.sessions.deinit();
    }

    /// Generate a random session token
    pub fn generateToken(self: *Self) ![32]u8 {
        _ = self;
        var token: [32]u8 = undefined;
        std.crypto.random.bytes(&token);
        return token;
    }

    /// Convert token bytes to hex string
    pub fn tokenToHex(token: [32]u8) [64]u8 {
        const hex_chars = "0123456789abcdef";
        var result: [64]u8 = undefined;
        for (token, 0..) |byte, i| {
            result[i * 2] = hex_chars[byte >> 4];
            result[i * 2 + 1] = hex_chars[byte & 0x0F];
        }
        return result;
    }

    /// Create a new session for authenticated user
    pub fn createSession(self: *Self, username: []const u8) ![]const u8 {
        const token = try self.generateToken();
        const hex = tokenToHex(token);
        const session_id = try self.allocator.dupe(u8, &hex);
        errdefer self.allocator.free(session_id);

        const now = std.time.timestamp();

        try self.sessions.put(session_id, .{
            .authenticated = true,
            .username = username,
            .created_at = now,
            .last_access = now,
        });

        return session_id;
    }

    /// Get session by ID
    pub fn getSession(self: *Self, session_id: []const u8) ?*Session {
        if (self.sessions.getPtr(session_id)) |session| {
            const now = std.time.timestamp();
            // Check if session expired
            if (now - session.last_access > SESSION_MAX_AGE) {
                self.destroySession(session_id);
                return null;
            }
            // Update last access time
            session.last_access = now;
            return session;
        }
        return null;
    }

    /// Check if session is authenticated
    pub fn isAuthenticated(self: *Self, session_id: ?[]const u8) bool {
        if (session_id) |sid| {
            if (self.getSession(sid)) |session| {
                return session.authenticated;
            }
        }
        return false;
    }

    /// Destroy a session
    pub fn destroySession(self: *Self, session_id: []const u8) void {
        if (self.sessions.fetchRemove(session_id)) |kv| {
            self.allocator.free(kv.key);
        }
    }

    /// Clean up expired sessions
    pub fn cleanupExpired(self: *Self) void {
        const now = std.time.timestamp();
        var to_remove: [100][]const u8 = undefined;
        var remove_count: usize = 0;

        var iter = self.sessions.iterator();
        while (iter.next()) |entry| {
            if (now - entry.value_ptr.last_access > SESSION_MAX_AGE) {
                if (remove_count < to_remove.len) {
                    to_remove[remove_count] = entry.key_ptr.*;
                    remove_count += 1;
                }
            }
        }

        for (to_remove[0..remove_count]) |sid| {
            self.destroySession(sid);
        }
    }
};

/// Parse session ID from Cookie header
pub fn getSessionIdFromCookies(cookie_header: ?[]const u8) ?[]const u8 {
    const header = cookie_header orelse return null;
    var cookies = std.mem.splitSequence(u8, header, "; ");
    while (cookies.next()) |cookie| {
        var parts = std.mem.splitScalar(u8, cookie, '=');
        const name = parts.next() orelse continue;
        const value = parts.next() orelse continue;
        if (std.mem.eql(u8, name, SessionManager.SESSION_COOKIE_NAME)) {
            return value;
        }
    }
    return null;
}

test "tokenToHex" {
    const token = [_]u8{ 0x00, 0x01, 0x02, 0xAB, 0xCD, 0xEF } ++ [_]u8{0} ** 26;
    const hex = SessionManager.tokenToHex(token);
    try std.testing.expect(std.mem.startsWith(u8, &hex, "000102abcdef"));
}
