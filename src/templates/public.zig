// Public templates - Homepage and playlist player
const std = @import("std");
const router = @import("../router.zig");
const layout = @import("layout.zig");
const db = @import("../db.zig");

/// Render homepage with playlist list
pub fn renderIndex(response: *router.Response, playlists: []const db.Database.Playlist) void {
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, "Home");
    layout.writePublicNav(response);

    response.write(
        \\    <div class="container mt-4">
        \\        <h1><i class="bi bi-collection-play me-2"></i>Playlists</h1>
        \\        <div class="row mt-4 g-4">
    );

    if (playlists.len == 0) {
        response.write("<div class=\"col-12\"><div class=\"alert alert-info\">No playlists available yet.</div></div>");
    } else {
        for (playlists) |playlist| {
            response.write("<div class=\"col-md-4 col-lg-3\">");
            response.write("<div class=\"card h-100 shadow-sm\">");
            response.write("<div class=\"card-body text-center\">");
            response.write("<i class=\"bi bi-music-note-list display-4 text-primary mb-3\"></i>");
            response.write("<h5 class=\"card-title\">");
            response.write(playlist.name);
            response.write("</h5>");
            response.write("<a href=\"/playlist/");
            response.write(playlist.slug);
            response.write("\" class=\"btn btn-primary\"><i class=\"bi bi-play-fill me-1\"></i>Play</a>");
            response.write("</div></div></div>");
        }
    }

    response.write(
        \\        </div>
        \\    </div>
    );
    layout.writeFooter(response);
}

/// Render playlist player page
pub fn renderPlayer(response: *router.Response, playlist: db.Database.Playlist, songs: []const db.Database.Song) void {
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, playlist.name);
    layout.writePublicNav(response);

    response.write("<div class=\"container mt-4\">");
    response.write("<a href=\"/\" class=\"btn btn-outline-secondary mb-3\"><i class=\"bi bi-arrow-left me-1\"></i>Back</a>");
    response.write("<h1><i class=\"bi bi-music-note-beamed me-2\"></i>");
    response.write(playlist.name);
    response.write("</h1>");

    // Player card
    response.write("<div class=\"card mt-4 shadow\">");
    response.write("<div class=\"card-body\">");

    // Audio player
    response.write("<div class=\"mb-4\">");
    response.write("<div id=\"nowPlaying\" class=\"text-center mb-2 text-muted\">Select a song to play</div>");
    response.write("<audio id=\"player\" controls class=\"w-100\"></audio>");
    response.write("</div>");

    // Song list
    if (songs.len == 0) {
        response.write("<div class=\"alert alert-info\">No songs in this playlist yet.</div>");
    } else {
        response.write("<div class=\"list-group\" id=\"songList\">");
        for (songs, 0..) |song, i| {
            response.write("<div class=\"list-group-item song-item d-flex justify-content-between align-items-center\" data-src=\"/mp3/");
            response.write(song.filename);
            response.write("\" data-title=\"");
            response.write(song.title);
            response.write("\" data-index=\"");
            var idx_buf: [10]u8 = undefined;
            const idx_str = std.fmt.bufPrint(&idx_buf, "{d}", .{i}) catch "0";
            response.write(idx_str);
            response.write("\">");
            response.write("<span><i class=\"bi bi-music-note me-2\"></i>");
            var pos_buf: [10]u8 = undefined;
            const pos_str = std.fmt.bufPrint(&pos_buf, "{d}", .{i + 1}) catch "?";
            response.write(pos_str);
            response.write(". ");
            response.write(song.title);
            response.write("</span>");
            response.write("<i class=\"bi bi-play-circle\"></i></div>");
        }
        response.write("</div>");
    }

    response.write("</div></div>");

    // Player script
    response.write(
        \\    <script>
        \\        const player = document.getElementById('player');
        \\        const nowPlaying = document.getElementById('nowPlaying');
        \\        const songs = document.querySelectorAll('.song-item');
        \\        let currentIndex = 0;
        \\        
        \\        function playSong(index) {
        \\            songs.forEach(s => {
        \\                s.classList.remove('playing');
        \\                s.querySelector('i:last-child').className = 'bi bi-play-circle';
        \\            });
        \\            const song = songs[index];
        \\            if (song) {
        \\                song.classList.add('playing');
        \\                song.querySelector('i:last-child').className = 'bi bi-pause-circle';
        \\                player.src = song.dataset.src;
        \\                nowPlaying.textContent = 'Now Playing: ' + song.dataset.title;
        \\                player.play();
        \\                currentIndex = index;
        \\            }
        \\        }
        \\        
        \\        songs.forEach((song, i) => {
        \\            song.addEventListener('click', () => {
        \\                if (currentIndex === i && !player.paused) {
        \\                    player.pause();
        \\                    song.querySelector('i:last-child').className = 'bi bi-play-circle';
        \\                } else {
        \\                    playSong(i);
        \\                }
        \\            });
        \\        });
        \\        
        \\        player.addEventListener('ended', () => {
        \\            if (currentIndex < songs.length - 1) {
        \\                playSong(currentIndex + 1);
        \\            }
        \\        });
        \\        
        \\        player.addEventListener('pause', () => {
        \\            const song = songs[currentIndex];
        \\            if (song) song.querySelector('i:last-child').className = 'bi bi-play-circle';
        \\        });
        \\        
        \\        player.addEventListener('play', () => {
        \\            const song = songs[currentIndex];
        \\            if (song) song.querySelector('i:last-child').className = 'bi bi-pause-circle';
        \\        });
        \\    </script>
    );

    response.write("</div>");
    layout.writeFooter(response);
}

/// Render 404 page
pub fn render404(response: *router.Response) void {
    response.setStatus(404, "Not Found");
    response.setContentType("text/html; charset=utf-8");
    layout.writeHead(response, "Not Found");
    response.write(
        \\<body class="d-flex align-items-center justify-content-center" style="min-height: 100vh;">
        \\    <div class="text-center">
        \\        <i class="bi bi-emoji-frown display-1 text-muted"></i>
        \\        <h1 class="mt-3">404</h1>
        \\        <p class="text-muted">Page not found</p>
        \\        <a href="/" class="btn btn-primary"><i class="bi bi-house me-1"></i>Go Home</a>
        \\    </div>
    );
    layout.writeFooter(response);
}
