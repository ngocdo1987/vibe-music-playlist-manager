// Router module - HTTP request routing and dispatch
const std = @import("std");

/// HTTP Method enum
pub const Method = enum {
    GET,
    POST,
    PUT,
    DELETE,

    pub fn fromString(s: []const u8) ?Method {
        if (std.mem.eql(u8, s, "GET")) return .GET;
        if (std.mem.eql(u8, s, "POST")) return .POST;
        if (std.mem.eql(u8, s, "PUT")) return .PUT;
        if (std.mem.eql(u8, s, "DELETE")) return .DELETE;
        return null;
    }
};

/// Parsed HTTP request
pub const Request = struct {
    method: Method,
    path: []const u8,
    headers: std.StringHashMap([]const u8),
    body: []const u8,

    /// Get cookie value from Cookie header
    pub fn getCookie(self: *const Request, name: []const u8) ?[]const u8 {
        const cookie_header = self.headers.get("Cookie") orelse return null;
        var cookies = std.mem.splitSequence(u8, cookie_header, "; ");
        while (cookies.next()) |cookie| {
            var parts = std.mem.splitScalar(u8, cookie, '=');
            const cookie_name = parts.next() orelse continue;
            const cookie_value = parts.next() orelse continue;
            if (std.mem.eql(u8, cookie_name, name)) {
                return cookie_value;
            }
        }
        return null;
    }
};

/// HTTP Response builder - uses fixed buffers for simplicity with Zig 0.15
pub const Response = struct {
    status: u16 = 200,
    status_text: []const u8 = "OK",
    header_buf: [4096]u8 = undefined,
    header_len: usize = 0,
    body_buf: [1024 * 1024]u8 = undefined, // 1MB max body
    body_len: usize = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Response {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Response) void {
        _ = self;
        // Nothing to free with fixed buffers
    }

    pub fn setStatus(self: *Response, status: u16, text: []const u8) void {
        self.status = status;
        self.status_text = text;
    }

    pub fn addHeader(self: *Response, name: []const u8, value: []const u8) void {
        for (name) |ch| {
            if (self.header_len < self.header_buf.len) {
                self.header_buf[self.header_len] = ch;
                self.header_len += 1;
            }
        }
        if (self.header_len + 2 < self.header_buf.len) {
            self.header_buf[self.header_len] = ':';
            self.header_buf[self.header_len + 1] = ' ';
            self.header_len += 2;
        }
        for (value) |ch| {
            if (self.header_len < self.header_buf.len) {
                self.header_buf[self.header_len] = ch;
                self.header_len += 1;
            }
        }
        if (self.header_len + 2 < self.header_buf.len) {
            self.header_buf[self.header_len] = '\r';
            self.header_buf[self.header_len + 1] = '\n';
            self.header_len += 2;
        }
    }

    pub fn setContentType(self: *Response, content_type: []const u8) void {
        self.addHeader("Content-Type", content_type);
    }

    pub fn setCookie(self: *Response, name: []const u8, value: []const u8) void {
        // Build cookie header inline
        self.addHeaderRaw("Set-Cookie: ");
        self.addHeaderRaw(name);
        self.addHeaderRaw("=");
        self.addHeaderRaw(value);
        self.addHeaderRaw("; Path=/; HttpOnly\r\n");
    }

    fn addHeaderRaw(self: *Response, data: []const u8) void {
        for (data) |ch| {
            if (self.header_len < self.header_buf.len) {
                self.header_buf[self.header_len] = ch;
                self.header_len += 1;
            }
        }
    }

    pub fn write(self: *Response, data: []const u8) void {
        for (data) |ch| {
            if (self.body_len < self.body_buf.len) {
                self.body_buf[self.body_len] = ch;
                self.body_len += 1;
            }
        }
    }

    pub fn redirect(self: *Response, location: []const u8) void {
        self.setStatus(302, "Found");
        self.addHeader("Location", location);
    }

    /// Build the complete HTTP response and write to stream
    pub fn send(self: *Response, stream: std.net.Stream) !void {
        // Status line
        var status_line: [64]u8 = undefined;
        const status_str = std.fmt.bufPrint(&status_line, "HTTP/1.1 {d} {s}\r\n", .{ self.status, self.status_text }) catch return;
        _ = try stream.write(status_str);

        // Headers
        _ = try stream.write(self.header_buf[0..self.header_len]);

        // Content-Length
        var len_line: [64]u8 = undefined;
        const len_str = std.fmt.bufPrint(&len_line, "Content-Length: {d}\r\n", .{self.body_len}) catch return;
        _ = try stream.write(len_str);

        // Connection close
        _ = try stream.write("Connection: close\r\n");

        // End of headers
        _ = try stream.write("\r\n");

        // Body
        _ = try stream.write(self.body_buf[0..self.body_len]);
    }
};

/// Check if a path matches a pattern (supports :param placeholders)
pub fn matchRoute(pattern: []const u8, path: []const u8) bool {
    var pattern_parts = std.mem.splitScalar(u8, pattern, '/');
    var path_parts = std.mem.splitScalar(u8, path, '/');

    while (true) {
        const p_part = pattern_parts.next();
        const path_part = path_parts.next();

        if (p_part == null and path_part == null) return true;
        if (p_part == null or path_part == null) return false;

        // Parameter placeholder matches any value
        if (p_part.?.len > 0 and p_part.?[0] == ':') continue;

        // Exact match required
        if (!std.mem.eql(u8, p_part.?, path_part.?)) return false;
    }
}

/// Parse raw HTTP request into Request struct
pub fn parseRequest(allocator: std.mem.Allocator, raw: []const u8) !Request {
    var headers = std.StringHashMap([]const u8).init(allocator);

    var lines = std.mem.splitSequence(u8, raw, "\r\n");

    // Parse request line
    const request_line = lines.next() orelse return error.InvalidRequest;
    var parts = std.mem.splitScalar(u8, request_line, ' ');
    const method_str = parts.next() orelse return error.InvalidRequest;
    const path = parts.next() orelse return error.InvalidRequest;

    const method = Method.fromString(method_str) orelse return error.InvalidMethod;

    // Parse headers
    while (lines.next()) |line| {
        if (line.len == 0) break; // Empty line = end of headers

        if (std.mem.indexOf(u8, line, ": ")) |sep| {
            const name = line[0..sep];
            const value = line[sep + 2 ..];
            try headers.put(name, value);
        }
    }

    // Rest is body
    const body = lines.rest();

    return Request{
        .method = method,
        .path = path,
        .headers = headers,
        .body = body,
    };
}

test "matchRoute exact" {
    try std.testing.expect(matchRoute("/", "/"));
    try std.testing.expect(matchRoute("/admin", "/admin"));
    try std.testing.expect(!matchRoute("/admin", "/admin/login"));
}

test "matchRoute with param" {
    try std.testing.expect(matchRoute("/playlist/:slug", "/playlist/rock-music"));
    try std.testing.expect(matchRoute("/admin/playlists/:id", "/admin/playlists/123"));
    try std.testing.expect(!matchRoute("/playlist/:slug", "/playlist/rock-music/edit"));
}
