// Layout template - Base HTML structure with Bootstrap and theme toggle
const std = @import("std");
const router = @import("../router.zig");

/// Write the HTML head with Bootstrap and common meta tags
pub fn writeHead(response: *router.Response, title: []const u8) void {
    response.write(
        \\<!DOCTYPE html>
        \\<html lang="en" data-bs-theme="dark">
        \\<head>
        \\    <meta charset="UTF-8">
        \\    <meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\    <title>
    );
    response.write(title);
    response.write(
        \\ - Music Manager</title>
        \\    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
        \\    <link href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.2/font/bootstrap-icons.min.css" rel="stylesheet">
        \\    <style>
        \\        .theme-toggle { cursor: pointer; }
        \\        .song-item { cursor: pointer; transition: background-color 0.2s; }
        \\        .song-item:hover { background-color: var(--bs-tertiary-bg); }
        \\        .song-item.playing { background-color: var(--bs-primary); color: white; }
        \\        .sortable-ghost { opacity: 0.4; }
        \\        .drag-handle { cursor: grab; }
        \\        .drag-handle:active { cursor: grabbing; }
        \\    </style>
        \\</head>
        \\<body>
    );
}

/// Write the public navbar
pub fn writePublicNav(response: *router.Response) void {
    response.write(
        \\    <nav class="navbar navbar-expand-lg navbar-dark bg-primary">
        \\        <div class="container">
        \\            <a class="navbar-brand" href="/"><i class="bi bi-music-note-beamed me-2"></i>Music Manager</a>
        \\            <div class="d-flex align-items-center">
        \\                <span class="theme-toggle text-light me-3" onclick="toggleTheme()" title="Toggle theme">
        \\                    <i class="bi bi-moon-fill" id="themeIcon"></i>
        \\                </span>
        \\                <a class="btn btn-outline-light btn-sm" href="/admin">Admin</a>
        \\            </div>
        \\        </div>
        \\    </nav>
    );
}

/// Write the admin navbar
pub fn writeAdminNav(response: *router.Response) void {
    response.write(
        \\    <nav class="navbar navbar-expand-lg navbar-dark bg-dark">
        \\        <div class="container">
        \\            <a class="navbar-brand" href="/admin"><i class="bi bi-gear-fill me-2"></i>Admin</a>
        \\            <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#adminNav">
        \\                <span class="navbar-toggler-icon"></span>
        \\            </button>
        \\            <div class="collapse navbar-collapse" id="adminNav">
        \\                <ul class="navbar-nav me-auto">
        \\                    <li class="nav-item"><a class="nav-link" href="/admin/playlists">Playlists</a></li>
        \\                </ul>
        \\                <div class="d-flex align-items-center">
        \\                    <span class="theme-toggle text-light me-3" onclick="toggleTheme()" title="Toggle theme">
        \\                        <i class="bi bi-moon-fill" id="themeIcon"></i>
        \\                    </span>
        \\                    <a class="nav-link text-light" href="/" target="_blank"><i class="bi bi-box-arrow-up-right me-1"></i>View Site</a>
        \\                    <a class="btn btn-outline-danger btn-sm ms-3" href="/admin/logout">Logout</a>
        \\                </div>
        \\            </div>
        \\        </div>
        \\    </nav>
    );
}

/// Write the footer with Bootstrap JS and theme toggle script
pub fn writeFooter(response: *router.Response) void {
    response.write(
        \\    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"></script>
        \\    <script>
        \\        function toggleTheme() {
        \\            const html = document.documentElement;
        \\            const icon = document.getElementById('themeIcon');
        \\            if (html.getAttribute('data-bs-theme') === 'dark') {
        \\                html.setAttribute('data-bs-theme', 'light');
        \\                icon.className = 'bi bi-sun-fill';
        \\                localStorage.setItem('theme', 'light');
        \\            } else {
        \\                html.setAttribute('data-bs-theme', 'dark');
        \\                icon.className = 'bi bi-moon-fill';
        \\                localStorage.setItem('theme', 'dark');
        \\            }
        \\        }
        \\        // Apply saved theme on load
        \\        (function() {
        \\            const saved = localStorage.getItem('theme');
        \\            if (saved === 'light') {
        \\                document.documentElement.setAttribute('data-bs-theme', 'light');
        \\                const icon = document.getElementById('themeIcon');
        \\                if (icon) icon.className = 'bi bi-sun-fill';
        \\            }
        \\        })();
        \\    </script>
        \\</body>
        \\</html>
    );
}

/// Write footer with SortableJS for admin drag-and-drop
pub fn writeAdminFooter(response: *router.Response) void {
    response.write(
        \\    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"></script>
        \\    <script src="https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"></script>
        \\    <script>
        \\        function toggleTheme() {
        \\            const html = document.documentElement;
        \\            const icon = document.getElementById('themeIcon');
        \\            if (html.getAttribute('data-bs-theme') === 'dark') {
        \\                html.setAttribute('data-bs-theme', 'light');
        \\                icon.className = 'bi bi-sun-fill';
        \\                localStorage.setItem('theme', 'light');
        \\            } else {
        \\                html.setAttribute('data-bs-theme', 'dark');
        \\                icon.className = 'bi bi-moon-fill';
        \\                localStorage.setItem('theme', 'dark');
        \\            }
        \\        }
        \\        (function() {
        \\            const saved = localStorage.getItem('theme');
        \\            if (saved === 'light') {
        \\                document.documentElement.setAttribute('data-bs-theme', 'light');
        \\                const icon = document.getElementById('themeIcon');
        \\                if (icon) icon.className = 'bi bi-sun-fill';
        \\            }
        \\        })();
        \\    </script>
        \\</body>
        \\</html>
    );
}

/// Write a centered card layout for login/error pages
pub fn writeCenteredCard(response: *router.Response, title: []const u8, content_fn: *const fn (*router.Response) void) void {
    writeHead(response, title);
    response.write(
        \\<body class="d-flex align-items-center py-4 bg-body-tertiary" style="min-height: 100vh;">
        \\    <main class="w-100 m-auto" style="max-width: 400px;">
        \\        <div class="card shadow-sm">
        \\            <div class="card-body p-4">
    );
    content_fn(response);
    response.write(
        \\            </div>
        \\        </div>
        \\    </main>
    );
    writeFooter(response);
}

/// Write an alert box
pub fn writeAlert(response: *router.Response, alert_type: []const u8, message: []const u8) void {
    response.write("<div class=\"alert alert-");
    response.write(alert_type);
    response.write(" alert-dismissible fade show\" role=\"alert\">");
    response.write(message);
    response.write("<button type=\"button\" class=\"btn-close\" data-bs-dismiss=\"alert\"></button></div>");
}
