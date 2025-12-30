// Music Manager - Pure Zig Web Application
// Entry point and HTTP server setup

const std = @import("std");
const db = @import("db.zig");
const router = @import("router.zig");
const auth = @import("auth.zig");
const admin_tpl = @import("templates/admin.zig");
const public_tpl = @import("templates/public.zig");
const multipart = @import("utils/multipart.zig");
const mp3 = @import("utils/mp3.zig");

// Global references (for handlers)
var g_database: ?*db.Database = null;
var g_auth: ?*auth.Auth = null;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Change to executable's directory so relative paths work correctly
    // The executable is in zig-out/bin/, so go up 2 levels to project root
    var exe_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fs.selfExeDirPath(&exe_dir_buf)) |exe_dir| {
        // Build path to project root (../../ from zig-out/bin)
        var root_buf: [std.fs.max_path_bytes]u8 = undefined;
        const root_path = std.fmt.bufPrint(&root_buf, "{s}/../..", .{exe_dir}) catch null;
        if (root_path) |path| {
            std.posix.chdir(path) catch |err| {
                std.debug.print("Warning: Could not change to project root: {}\n", .{err});
            };
        }
    } else |_| {
        std.debug.print("Warning: Could not determine executable directory\n", .{});
    }

    // Initialize database
    var database = try db.Database.init(allocator, "music_manager.db");
    defer database.deinit();
    g_database = &database;

    try database.createTables();
    std.debug.print("Database initialized\n", .{});

    // Initialize authentication
    var authentication = try auth.Auth.init(allocator);
    defer authentication.deinit();
    g_auth = &authentication;
    std.debug.print("Auth initialized (user: {s})\n", .{authentication.config.admin_username});

    // Create TCP listener
    const port = authentication.config.server_port;
    const address = std.net.Address.parseIp("0.0.0.0", port) catch unreachable;
    var listener = try std.net.Address.listen(address, .{
        .reuse_address = true,
    });
    defer listener.deinit();

    std.debug.print("Music Manager running at http://localhost:{d}\n", .{port});

    while (true) {
        const connection = listener.accept() catch |err| {
            std.debug.print("Accept error: {}\n", .{err});
            continue;
        };

        handleConnection(allocator, connection.stream) catch |err| {
            std.debug.print("Handler error: {}\n", .{err});
        };
    }
}

fn handleConnection(allocator: std.mem.Allocator, stream: std.net.Stream) !void {
    defer stream.close();

    // Allocate a larger buffer for file uploads (10MB max)
    const max_size = 10 * 1024 * 1024;
    var buf = try allocator.alloc(u8, max_size);
    defer allocator.free(buf);

    // Read initial data
    var total_len: usize = 0;
    const initial_len = try stream.read(buf);
    if (initial_len == 0) return;
    total_len = initial_len;

    // Check Content-Length header to know if we need to read more
    const header_end = std.mem.indexOf(u8, buf[0..total_len], "\r\n\r\n");
    if (header_end) |h_end| {
        // Look for Content-Length in headers
        const headers_section = buf[0..h_end];
        if (std.mem.indexOf(u8, headers_section, "Content-Length: ")) |cl_start| {
            const cl_value_start = cl_start + "Content-Length: ".len;
            var cl_end = cl_value_start;
            while (cl_end < headers_section.len and headers_section[cl_end] != '\r') : (cl_end += 1) {}
            const cl_str = headers_section[cl_value_start..cl_end];
            if (std.fmt.parseInt(usize, cl_str, 10)) |content_length| {
                const body_start = h_end + 4;
                const expected_total = body_start + content_length;

                // Read more data if needed
                while (total_len < expected_total and total_len < max_size) {
                    const read_len = stream.read(buf[total_len..]) catch break;
                    if (read_len == 0) break;
                    total_len += read_len;
                }
            } else |_| {}
        }
    }

    var request = router.parseRequest(allocator, buf[0..total_len]) catch {
        _ = try stream.write("HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\nBad Request\n");
        return;
    };
    defer request.headers.deinit();

    // Stream MP3 files directly (bypass Response buffer for large files)
    if (std.mem.startsWith(u8, request.path, "/mp3/")) {
        try streamMp3File(allocator, request.path, stream);
        return;
    }

    var response = router.Response.init(allocator);
    defer response.deinit();

    routeRequest(allocator, &request, &response) catch |err| {
        std.debug.print("Route error: {}\n", .{err});
        response.setStatus(500, "Internal Server Error");
        response.write("Internal Server Error\n");
    };

    try response.send(stream);
}

/// Stream MP3 file directly to client (bypasses Response buffer for large files)
fn streamMp3File(allocator: std.mem.Allocator, path: []const u8, stream: std.net.Stream) !void {
    var file_path_buf: [256]u8 = undefined;
    const file_path = std.fmt.bufPrint(&file_path_buf, "public{s}", .{path}) catch {
        _ = try stream.write("HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\nNot Found\n");
        return;
    };

    const file = std.fs.cwd().openFile(file_path, .{}) catch {
        _ = try stream.write("HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\nNot Found\n");
        return;
    };
    defer file.close();

    // Get file size
    const stat = try file.stat();
    const file_size = stat.size;

    // Write HTTP headers
    var header_buf: [256]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "HTTP/1.1 200 OK\r\nContent-Type: audio/mpeg\r\nContent-Length: {d}\r\nAccept-Ranges: bytes\r\nConnection: close\r\n\r\n", .{file_size}) catch {
        _ = try stream.write("HTTP/1.1 500 Internal Server Error\r\n\r\n");
        return;
    };
    _ = try stream.write(header);

    // Stream file content in chunks
    var buf: [64 * 1024]u8 = undefined; // 64KB chunks
    while (true) {
        const bytes_read = file.read(&buf) catch break;
        if (bytes_read == 0) break;
        _ = stream.write(buf[0..bytes_read]) catch break;
    }

    _ = allocator;
}

fn routeRequest(allocator: std.mem.Allocator, request: *const router.Request, response: *router.Response) !void {
    const path = request.path;

    // MP3 files are now streamed directly in handleConnection
    if (std.mem.startsWith(u8, path, "/mp3/")) {
        // This path is handled by streamMp3File in handleConnection
        // Should not reach here, but just in case:
        public_tpl.render404(response);
        return;
    }

    switch (request.method) {
        .GET => {
            if (std.mem.eql(u8, path, "/")) {
                try handleIndex(allocator, response);
            } else if (std.mem.startsWith(u8, path, "/playlist/")) {
                try handlePlaylist(allocator, request, response);
            } else if (std.mem.eql(u8, path, "/admin/login")) {
                admin_tpl.renderLogin(response, null);
            } else if (std.mem.eql(u8, path, "/admin/logout")) {
                handleLogout(request, response);
            } else if (std.mem.startsWith(u8, path, "/admin")) {
                if (!isAuthenticated(request)) {
                    response.redirect("/admin/login");
                } else if (std.mem.eql(u8, path, "/admin") or std.mem.eql(u8, path, "/admin/")) {
                    try handleDashboard(allocator, response);
                } else if (std.mem.eql(u8, path, "/admin/playlists")) {
                    try handleAdminPlaylists(allocator, response);
                } else if (std.mem.eql(u8, path, "/admin/playlists/new")) {
                    admin_tpl.renderNewPlaylist(response);
                } else if (std.mem.startsWith(u8, path, "/admin/playlists/")) {
                    try handleAdminEditPlaylist(allocator, path, response);
                } else {
                    public_tpl.render404(response);
                }
            } else {
                public_tpl.render404(response);
            }
        },
        .POST => {
            if (std.mem.eql(u8, path, "/admin/login")) {
                try handleLogin(allocator, request, response);
            } else if (std.mem.eql(u8, path, "/admin/playlists") and isAuthenticated(request)) {
                try handleCreatePlaylist(allocator, request, response);
            } else if (std.mem.startsWith(u8, path, "/admin/playlists/") and isAuthenticated(request)) {
                try handlePlaylistPost(allocator, path, request, response);
            } else {
                public_tpl.render404(response);
            }
        },
        else => public_tpl.render404(response),
    }
}

fn isAuthenticated(request: *const router.Request) bool {
    if (g_auth) |authentication| {
        return authentication.isAuthenticated(request.headers.get("Cookie"));
    }
    return false;
}

// ==================== Public Handlers ====================

fn handleIndex(allocator: std.mem.Allocator, response: *router.Response) !void {
    if (g_database) |database| {
        var playlists = try database.getAllPlaylists(allocator);
        defer playlists.deinit();
        public_tpl.renderIndex(response, playlists.items);
    }
}

fn handlePlaylist(allocator: std.mem.Allocator, request: *const router.Request, response: *router.Response) !void {
    const path = request.path;
    const slug = if (std.mem.lastIndexOf(u8, path, "/")) |idx| path[idx + 1 ..] else {
        public_tpl.render404(response);
        return;
    };

    if (g_database) |database| {
        const playlist = try database.getPlaylistBySlug(allocator, slug) orelse {
            public_tpl.render404(response);
            return;
        };
        var songs = try database.getSongsForPlaylist(allocator, playlist.id);
        defer songs.deinit();
        public_tpl.renderPlayer(response, playlist, songs.items);
    }
}

// ==================== Admin Handlers ====================

fn handleLogin(allocator: std.mem.Allocator, request: *const router.Request, response: *router.Response) !void {
    if (g_auth) |authentication| {
        var form = try auth.parseFormData(allocator, request.body);
        defer form.deinit();

        const username = form.get("username") orelse "";
        const password = form.get("password") orelse "";

        if (try authentication.login(username, password)) |session_id| {
            response.setCookie("session_id", session_id);
            response.redirect("/admin");
        } else {
            admin_tpl.renderLogin(response, "Invalid username or password");
        }
    } else {
        response.redirect("/admin/login");
    }
}

fn handleLogout(request: *const router.Request, response: *router.Response) void {
    if (g_auth) |authentication| {
        authentication.logout(request.headers.get("Cookie"));
    }
    response.addHeader("Set-Cookie", "session_id=; Path=/; HttpOnly; Max-Age=0");
    response.redirect("/admin/login");
}

fn handleDashboard(allocator: std.mem.Allocator, response: *router.Response) !void {
    if (g_database) |database| {
        var playlists = try database.getAllPlaylists(allocator);
        defer playlists.deinit();
        // TODO: get song count
        admin_tpl.renderDashboard(response, playlists.items.len, 0);
    }
}

fn handleAdminPlaylists(allocator: std.mem.Allocator, response: *router.Response) !void {
    if (g_database) |database| {
        var playlists = try database.getAllPlaylists(allocator);
        defer playlists.deinit();
        admin_tpl.renderPlaylistList(response, playlists.items);
    }
}

fn handleAdminEditPlaylist(allocator: std.mem.Allocator, path: []const u8, response: *router.Response) !void {
    // Extract ID from /admin/playlists/:id
    const id_start = "/admin/playlists/".len;
    if (path.len <= id_start) {
        public_tpl.render404(response);
        return;
    }

    // Find end of ID (before /delete or other suffix)
    var id_end = path.len;
    if (std.mem.indexOf(u8, path[id_start..], "/")) |slash| {
        id_end = id_start + slash;
    }

    const id_str = path[id_start..id_end];
    const id = std.fmt.parseInt(i64, id_str, 10) catch {
        public_tpl.render404(response);
        return;
    };

    // Check for /delete suffix
    if (std.mem.endsWith(u8, path, "/delete")) {
        if (g_database) |database| {
            try database.deletePlaylist(id);
        }
        response.redirect("/admin/playlists");
        return;
    }

    // Show edit page
    if (g_database) |database| {
        // Get playlist by ID (TODO: add getPlaylistById to db.zig)
        var playlists = try database.getAllPlaylists(allocator);
        defer playlists.deinit();

        for (playlists.items) |playlist| {
            if (playlist.id == id) {
                var songs = try database.getSongsForPlaylist(allocator, id);
                defer songs.deinit();
                admin_tpl.renderEditPlaylist(response, playlist, songs.items);
                return;
            }
        }
    }
    public_tpl.render404(response);
}

fn handleCreatePlaylist(allocator: std.mem.Allocator, request: *const router.Request, response: *router.Response) !void {
    if (g_database) |database| {
        var form = try auth.parseFormData(allocator, request.body);
        defer form.deinit();

        const name = form.get("name") orelse "";
        if (name.len > 0) {
            const slug = try db.generateSlug(allocator, name);
            defer allocator.free(slug);
            _ = try database.createPlaylist(allocator, name, slug);
        }
    }
    response.redirect("/admin/playlists");
}

fn handlePlaylistPost(allocator: std.mem.Allocator, path: []const u8, request: *const router.Request, response: *router.Response) !void {
    // Extract playlist ID
    const id_start = "/admin/playlists/".len;
    if (path.len <= id_start) {
        response.redirect("/admin/playlists");
        return;
    }

    var id_end = path.len;
    if (std.mem.indexOf(u8, path[id_start..], "/")) |slash| {
        id_end = id_start + slash;
    }

    const id_str = path[id_start..id_end];
    const playlist_id = std.fmt.parseInt(i64, id_str, 10) catch {
        response.redirect("/admin/playlists");
        return;
    };

    // Check for /reorder suffix (AJAX JSON)
    if (std.mem.endsWith(u8, path, "/reorder")) {
        try handleSongReorder(allocator, playlist_id, request, response);
        return;
    }

    // Check for /upload suffix (file upload)
    if (std.mem.endsWith(u8, path, "/upload")) {
        try handleFileUpload(allocator, playlist_id, request, response);
        return;
    }

    // Check for /songs/:song_id/delete
    if (std.mem.indexOf(u8, path, "/songs/")) |songs_idx| {
        const song_id_start = songs_idx + "/songs/".len;
        var song_id_end = path.len;
        if (std.mem.indexOf(u8, path[song_id_start..], "/")) |slash| {
            song_id_end = song_id_start + slash;
        }
        const song_id_str = path[song_id_start..song_id_end];
        const song_id = std.fmt.parseInt(i64, song_id_str, 10) catch {
            response.redirect("/admin/playlists");
            return;
        };

        if (std.mem.endsWith(u8, path, "/delete")) {
            // Remove song from playlist
            if (g_database) |database| {
                try database.removeSongFromPlaylist(playlist_id, song_id);
            }
        }
        var redirect_buf: [64]u8 = undefined;
        const redirect_url = std.fmt.bufPrint(&redirect_buf, "/admin/playlists/{d}", .{playlist_id}) catch "/admin/playlists";
        response.redirect(redirect_url);
        return;
    }

    // Otherwise it's an update (form POST)
    if (g_database) |database| {
        var form = try auth.parseFormData(allocator, request.body);
        defer form.deinit();

        const name = form.get("name") orelse "";
        if (name.len > 0) {
            const slug = try db.generateSlug(allocator, name);
            defer allocator.free(slug);
            try database.updatePlaylist(playlist_id, name, slug);
        }
    }

    var redirect_buf: [64]u8 = undefined;
    const redirect_url = std.fmt.bufPrint(&redirect_buf, "/admin/playlists/{d}", .{playlist_id}) catch "/admin/playlists";
    response.redirect(redirect_url);
}

fn handleSongReorder(allocator: std.mem.Allocator, playlist_id: i64, request: *const router.Request, response: *router.Response) !void {
    // Parse JSON body: {"song_ids": ["1", "2", "3"]}
    // Simple JSON parsing for our specific format
    const body = request.body;

    // Find song_ids array
    if (std.mem.indexOf(u8, body, "[")) |start| {
        if (std.mem.indexOf(u8, body, "]")) |end| {
            const ids_str = body[start + 1 .. end];

            // Parse comma-separated values
            var position: i32 = 0;
            var ids = std.mem.splitScalar(u8, ids_str, ',');
            while (ids.next()) |id_raw| {
                // Trim quotes and whitespace
                const id_trimmed = std.mem.trim(u8, id_raw, " \t\n\r\"'");
                if (id_trimmed.len == 0) continue;

                const song_id = std.fmt.parseInt(i64, id_trimmed, 10) catch continue;

                if (g_database) |database| {
                    database.updateSongPosition(playlist_id, song_id, position) catch {};
                }
                position += 1;
            }
        }
    }

    // Return JSON success response
    response.setContentType("application/json");
    response.write("{\"success\": true}");
    _ = allocator;
}

fn handleFileUpload(allocator: std.mem.Allocator, playlist_id: i64, request: *const router.Request, response: *router.Response) !void {
    // Get Content-Type header for boundary
    const content_type = request.headers.get("Content-Type") orelse {
        response.setStatus(400, "Bad Request");
        response.write("Missing Content-Type header");
        return;
    };

    // Parse multipart form data
    var form_data = multipart.parse(allocator, content_type, request.body) catch {
        response.setStatus(400, "Bad Request");
        response.write("Failed to parse form data");
        return;
    };
    defer form_data.deinit();

    // Process uploaded files
    for (form_data.files) |file| {
        // Validate MP3
        if (!mp3.isValidMp3(file.data)) {
            response.setStatus(400, "Bad Request");
            response.write("Invalid MP3 file");
            return;
        }

        // Extract only basename (remove any path components)
        var basename = file.filename;
        if (std.mem.lastIndexOf(u8, basename, "/")) |idx| {
            basename = basename[idx + 1 ..];
        }
        if (std.mem.lastIndexOf(u8, basename, "\\")) |idx| {
            basename = basename[idx + 1 ..];
        }

        // Sanitize and make unique filename
        const safe_filename = mp3.sanitizeFilename(allocator, basename) catch {
            response.setStatus(500, "Internal Server Error");
            return;
        };
        defer allocator.free(safe_filename);

        const unique_filename = mp3.generateUniqueFilename(allocator, safe_filename) catch {
            response.setStatus(500, "Internal Server Error");
            return;
        };
        defer allocator.free(unique_filename);

        // Save file to public/mp3/
        var file_path_buf: [512]u8 = undefined;
        const file_path = std.fmt.bufPrint(&file_path_buf, "public/mp3/{s}", .{unique_filename}) catch {
            response.setStatus(500, "Internal Server Error");
            return;
        };

        // Ensure directory exists
        std.fs.cwd().makePath("public/mp3") catch {};

        // Write file
        const out_file = std.fs.cwd().createFile(file_path, .{}) catch {
            response.setStatus(500, "Internal Server Error");
            response.write("Failed to save file");
            return;
        };
        defer out_file.close();
        out_file.writeAll(file.data) catch {
            response.setStatus(500, "Internal Server Error");
            return;
        };

        // Extract title and create song record
        const title = mp3.extractTitle(file.data, file.filename);

        if (g_database) |database| {
            // Create song in database
            const song_id = database.createSong(allocator, title, unique_filename, file_path) catch {
                response.setStatus(500, "Internal Server Error");
                return;
            };

            // Get current song count for position
            var songs = database.getSongsForPlaylist(allocator, playlist_id) catch {
                response.setStatus(500, "Internal Server Error");
                return;
            };
            defer songs.deinit();
            const position: i32 = @intCast(songs.items.len);

            // Add to playlist
            database.addSongToPlaylist(playlist_id, song_id, position) catch {};
        }
    }

    // Redirect back to edit page
    var redirect_buf: [64]u8 = undefined;
    const redirect_url = std.fmt.bufPrint(&redirect_buf, "/admin/playlists/{d}", .{playlist_id}) catch "/admin/playlists";
    response.redirect(redirect_url);
}

fn serveStaticFile(allocator: std.mem.Allocator, path: []const u8, response: *router.Response) !void {
    var file_path_buf: [256]u8 = undefined;
    const file_path = std.fmt.bufPrint(&file_path_buf, "public{s}", .{path}) catch {
        public_tpl.render404(response);
        return;
    };

    const file = std.fs.cwd().openFile(file_path, .{}) catch {
        public_tpl.render404(response);
        return;
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch {
        public_tpl.render404(response);
        return;
    };
    defer allocator.free(content);

    if (std.mem.endsWith(u8, path, ".mp3")) {
        response.setContentType("audio/mpeg");
    } else if (std.mem.endsWith(u8, path, ".css")) {
        response.setContentType("text/css");
    } else if (std.mem.endsWith(u8, path, ".js")) {
        response.setContentType("application/javascript");
    } else {
        response.setContentType("application/octet-stream");
    }

    response.write(content);
}

test "basic test" {
    try std.testing.expect(true);
}
