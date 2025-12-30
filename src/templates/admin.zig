// Admin templates - Dashboard, playlists, forms
const std = @import("std");
const router = @import("../router.zig");
const layout = @import("layout.zig");
const db = @import("../db.zig");

/// Render admin login page
pub fn renderLogin(response: *router.Response, error_msg: ?[]const u8) void {
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, "Admin Login");
    response.write(
        \\<body class="d-flex align-items-center py-4 bg-body-tertiary" style="min-height: 100vh;">
        \\    <main class="w-100 m-auto" style="max-width: 360px;">
        \\        <form method="POST" action="/admin/login" class="text-center">
        \\            <h1 class="h3 mb-4 fw-normal"><i class="bi bi-music-note-beamed me-2"></i>Admin Login</h1>
    );

    if (error_msg) |msg| {
        layout.writeAlert(response, "danger", msg);
    }

    response.write(
        \\            <div class="form-floating mb-2">
        \\                <input type="text" class="form-control" id="username" name="username" placeholder="Username" required autofocus>
        \\                <label for="username">Username</label>
        \\            </div>
        \\            <div class="form-floating mb-3">
        \\                <input type="password" class="form-control" id="password" name="password" placeholder="Password" required>
        \\                <label for="password">Password</label>
        \\            </div>
        \\            <button class="btn btn-primary w-100 py-2" type="submit"><i class="bi bi-box-arrow-in-right me-2"></i>Sign in</button>
        \\        </form>
        \\    </main>
    );
    layout.writeFooter(response);
}

/// Render admin dashboard
pub fn renderDashboard(response: *router.Response, playlist_count: usize, song_count: usize) void {
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, "Dashboard");
    layout.writeAdminNav(response);
    response.write(
        \\    <div class="container mt-4">
        \\        <h1><i class="bi bi-speedometer2 me-2"></i>Dashboard</h1>
        \\        <div class="row mt-4 g-4">
        \\            <div class="col-md-6">
        \\                <div class="card h-100">
        \\                    <div class="card-body">
        \\                        <div class="d-flex align-items-center">
        \\                            <i class="bi bi-collection-play fs-1 text-primary me-3"></i>
        \\                            <div>
        \\                                <h5 class="card-title mb-0">Playlists</h5>
        \\                                <p class="card-text fs-3 mb-0">
    );
    var count_buf: [20]u8 = undefined;
    const count_str = std.fmt.bufPrint(&count_buf, "{d}", .{playlist_count}) catch "0";
    response.write(count_str);
    response.write(
        \\</p>
        \\                            </div>
        \\                        </div>
        \\                        <a href="/admin/playlists" class="btn btn-primary mt-3">Manage Playlists</a>
        \\                    </div>
        \\                </div>
        \\            </div>
        \\            <div class="col-md-6">
        \\                <div class="card h-100">
        \\                    <div class="card-body">
        \\                        <div class="d-flex align-items-center">
        \\                            <i class="bi bi-music-note-list fs-1 text-success me-3"></i>
        \\                            <div>
        \\                                <h5 class="card-title mb-0">Total Songs</h5>
        \\                                <p class="card-text fs-3 mb-0">
    );
    const song_str = std.fmt.bufPrint(&count_buf, "{d}", .{song_count}) catch "0";
    response.write(song_str);
    response.write(
        \\</p>
        \\                            </div>
        \\                        </div>
        \\                    </div>
        \\                </div>
        \\            </div>
        \\        </div>
        \\    </div>
    );
    layout.writeAdminFooter(response);
}

/// Render playlist list page
pub fn renderPlaylistList(response: *router.Response, playlists: []const db.Database.Playlist) void {
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, "Playlists");
    layout.writeAdminNav(response);
    response.write(
        \\    <div class="container mt-4">
        \\        <div class="d-flex justify-content-between align-items-center mb-4">
        \\            <h1><i class="bi bi-collection-play me-2"></i>Playlists</h1>
        \\            <a href="/admin/playlists/new" class="btn btn-success"><i class="bi bi-plus-lg me-1"></i>New Playlist</a>
        \\        </div>
    );

    if (playlists.len == 0) {
        response.write("<div class=\"alert alert-info\">No playlists yet. Create your first one!</div>");
    } else {
        response.write("<div class=\"list-group\">");
        for (playlists) |playlist| {
            response.write("<div class=\"list-group-item d-flex justify-content-between align-items-center\">");
            response.write("<div><i class=\"bi bi-music-note-list me-2\"></i>");
            response.write(playlist.name);
            response.write("<small class=\"text-muted ms-2\">(/playlist/");
            response.write(playlist.slug);
            response.write(")</small></div>");
            response.write("<div class=\"btn-group btn-group-sm\">");
            response.write("<a href=\"/admin/playlists/");
            var id_buf: [20]u8 = undefined;
            const id_str = std.fmt.bufPrint(&id_buf, "{d}", .{playlist.id}) catch "0";
            response.write(id_str);
            response.write("\" class=\"btn btn-outline-primary\"><i class=\"bi bi-pencil\"></i></a>");
            response.write("<a href=\"/admin/playlists/");
            response.write(id_str);
            response.write("/delete\" class=\"btn btn-outline-danger\" onclick=\"return confirm('Delete this playlist?')\"><i class=\"bi bi-trash\"></i></a>");
            response.write("</div></div>");
        }
        response.write("</div>");
    }

    response.write("    </div>");
    layout.writeAdminFooter(response);
}

/// Render new playlist form
pub fn renderNewPlaylist(response: *router.Response) void {
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, "New Playlist");
    layout.writeAdminNav(response);
    response.write(
        \\    <div class="container mt-4">
        \\        <h1><i class="bi bi-plus-circle me-2"></i>New Playlist</h1>
        \\        <form method="POST" action="/admin/playlists" class="mt-4" style="max-width: 500px;">
        \\            <div class="mb-3">
        \\                <label for="name" class="form-label">Playlist Name</label>
        \\                <input type="text" class="form-control" id="name" name="name" required autofocus>
        \\            </div>
        \\            <button type="submit" class="btn btn-primary"><i class="bi bi-check-lg me-1"></i>Create</button>
        \\            <a href="/admin/playlists" class="btn btn-secondary">Cancel</a>
        \\        </form>
        \\    </div>
    );
    layout.writeAdminFooter(response);
}

/// Render edit playlist page with songs
pub fn renderEditPlaylist(response: *router.Response, playlist: db.Database.Playlist, songs: []const db.Database.Song) void {
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, "Edit Playlist");
    layout.writeAdminNav(response);

    response.write("<div class=\"container mt-4\">");
    response.write("<h1><i class=\"bi bi-pencil me-2\"></i>Edit: ");
    response.write(playlist.name);
    response.write("</h1>");

    // Edit form
    response.write("<form method=\"POST\" action=\"/admin/playlists/");
    var id_buf: [20]u8 = undefined;
    const id_str = std.fmt.bufPrint(&id_buf, "{d}", .{playlist.id}) catch "0";
    response.write(id_str);
    response.write("\" class=\"mt-4\" style=\"max-width: 500px;\">");
    response.write("<div class=\"mb-3\"><label for=\"name\" class=\"form-label\">Playlist Name</label>");
    response.write("<input type=\"text\" class=\"form-control\" id=\"name\" name=\"name\" value=\"");
    response.write(playlist.name);
    response.write("\" required></div>");
    response.write("<button type=\"submit\" class=\"btn btn-primary\"><i class=\"bi bi-check-lg me-1\"></i>Save</button>");
    response.write("<a href=\"/admin/playlists\" class=\"btn btn-secondary ms-2\">Back</a></form>");

    // Songs section
    response.write("<hr class=\"my-4\"><h3><i class=\"bi bi-music-note-list me-2\"></i>Songs</h3>");
    response.write("<form method=\"POST\" action=\"/admin/playlists/");
    response.write(id_str);
    response.write("/upload\" enctype=\"multipart/form-data\" class=\"mb-3\">");
    response.write("<div class=\"input-group\"><input type=\"file\" class=\"form-control\" name=\"mp3\" accept=\".mp3\" required>");
    response.write("<button type=\"submit\" class=\"btn btn-success\"><i class=\"bi bi-upload me-1\"></i>Upload MP3</button></div></form>");

    if (songs.len == 0) {
        response.write("<div class=\"alert alert-info\">No songs in this playlist yet. Upload some MP3s!</div>");
    } else {
        response.write("<div class=\"list-group\" id=\"songList\">");
        for (songs, 0..) |song, i| {
            response.write("<div class=\"list-group-item d-flex justify-content-between align-items-center\" data-id=\"");
            var song_id_buf: [20]u8 = undefined;
            const song_id_str = std.fmt.bufPrint(&song_id_buf, "{d}", .{song.id}) catch "0";
            response.write(song_id_str);
            response.write("\">");
            response.write("<div><span class=\"drag-handle me-2\"><i class=\"bi bi-grip-vertical\"></i></span>");
            var pos_buf: [10]u8 = undefined;
            const pos_str = std.fmt.bufPrint(&pos_buf, "{d}", .{i + 1}) catch "?";
            response.write(pos_str);
            response.write(". ");
            response.write(song.title);
            response.write("</div>");
            response.write("<a href=\"/admin/playlists/");
            response.write(id_str);
            response.write("/songs/");
            response.write(song_id_str);
            response.write("/delete\" class=\"btn btn-sm btn-outline-danger\"><i class=\"bi bi-trash\"></i></a></div>");
        }
        response.write("</div>");

        // Sortable script
        response.write("<script>document.addEventListener('DOMContentLoaded', function() {");
        response.write("new Sortable(document.getElementById('songList'), {");
        response.write("handle: '.drag-handle', animation: 150, onEnd: function(evt) {");
        response.write("const ids = Array.from(document.querySelectorAll('#songList [data-id]')).map(el => el.dataset.id);");
        response.write("fetch('/admin/playlists/");
        response.write(id_str);
        response.write("/reorder', {method: 'POST', headers: {'Content-Type': 'application/json'}, body: JSON.stringify({song_ids: ids})});");
        response.write("}});});</script>");
    }

    response.write("</div>");
    layout.writeAdminFooter(response);
}
