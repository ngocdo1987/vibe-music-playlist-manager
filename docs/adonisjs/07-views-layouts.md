# Views - Layouts

## Admin Layout

File: `resources/views/layouts/admin.edge`

\`\`\`html
<!DOCTYPE html>
<html lang="en" data-bs-theme="light">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{{ title || 'Admin - Music Playlist' }}</title>
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css" rel="stylesheet">
  <style>
    .sidebar {
      min-height: 100vh;
      background-color: var(--bs-tertiary-bg);
    }
    .song-item {
      cursor: grab;
      transition: background-color 0.2s;
    }
    .song-item:active {
      cursor: grabbing;
    }
    .song-item.dragging {
      opacity: 0.5;
      background-color: var(--bs-primary-bg-subtle);
    }
    .drop-zone {
      border: 2px dashed var(--bs-border-color);
      border-radius: 8px;
      padding: 2rem;
      text-align: center;
      transition: all 0.3s;
    }
    .drop-zone.drag-over {
      border-color: var(--bs-primary);
      background-color: var(--bs-primary-bg-subtle);
    }
  </style>
</head>
<body>
  <div class="d-flex">
    <!-- Sidebar -->
    <nav class="sidebar d-flex flex-column p-3" style="width: 250px;">
      <a href="/admin/dashboard" class="d-flex align-items-center mb-3 text-decoration-none">
        <i class="bi bi-music-note-list fs-4 me-2"></i>
        <span class="fs-5 fw-semibold">Music Admin</span>
      </a>
      <hr>
      <ul class="nav nav-pills flex-column mb-auto">
        <li class="nav-item">
          <a href="/admin/dashboard" class="nav-link {{ request.url() === '/admin/dashboard' ? 'active' : '' }}">
            <i class="bi bi-speedometer2 me-2"></i>
            Dashboard
          </a>
        </li>
        <li class="nav-item">
          <a href="/admin/playlists" class="nav-link {{ request.url().includes('/admin/playlists') ? 'active' : '' }}">
            <i class="bi bi-collection me-2"></i>
            Playlists
          </a>
        </li>
      </ul>
      <hr>
      <div class="d-flex align-items-center justify-content-between">
        <button class="btn btn-sm btn-outline-secondary" id="themeToggle">
          <i class="bi bi-moon-fill" id="themeIcon"></i>
        </button>
        <form action="/admin/logout" method="POST" class="d-inline">
          <button type="submit" class="btn btn-sm btn-outline-danger">
            <i class="bi bi-box-arrow-right me-1"></i>Logout
          </button>
        </form>
      </div>
    </nav>

    <!-- Main Content -->
    <main class="flex-grow-1 p-4">
      @if(flashMessages.has('success'))
        <div class="alert alert-success alert-dismissible fade show" role="alert">
          {{ flashMessages.get('success') }}
          <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
        </div>
      @endif
      
      @if(flashMessages.has('error'))
        <div class="alert alert-danger alert-dismissible fade show" role="alert">
          {{ flashMessages.get('error') }}
          <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
        </div>
      @endif

      @!section('content')
    </main>
  </div>

  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"></script>
  <script>
    // Theme Toggle
    const themeToggle = document.getElementById('themeToggle');
    const themeIcon = document.getElementById('themeIcon');
    const html = document.documentElement;
    
    // Load saved theme
    const savedTheme = localStorage.getItem('theme') || 'light';
    html.setAttribute('data-bs-theme', savedTheme);
    updateThemeIcon(savedTheme);
    
    themeToggle.addEventListener('click', () => {
      const currentTheme = html.getAttribute('data-bs-theme');
      const newTheme = currentTheme === 'light' ? 'dark' : 'light';
      html.setAttribute('data-bs-theme', newTheme);
      localStorage.setItem('theme', newTheme);
      updateThemeIcon(newTheme);
    });
    
    function updateThemeIcon(theme) {
      themeIcon.className = theme === 'light' ? 'bi bi-moon-fill' : 'bi bi-sun-fill';
    }
  </script>
  @!section('scripts')
</body>
</html>
\`\`\`

## Frontend Layout

File: `resources/views/layouts/frontend.edge`

\`\`\`html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{{ title || 'Music Playlist' }}</title>
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css" rel="stylesheet">
  <style>
    body {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      min-height: 100vh;
    }
    .card {
      backdrop-filter: blur(10px);
      background-color: rgba(255, 255, 255, 0.95);
    }
    .playlist-card {
      transition: transform 0.2s, box-shadow 0.2s;
    }
    .playlist-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2);
    }
    .player-container {
      position: sticky;
      bottom: 0;
      background: rgba(255, 255, 255, 0.98);
      backdrop-filter: blur(10px);
    }
    .song-list-item {
      cursor: pointer;
      transition: background-color 0.2s;
    }
    .song-list-item:hover {
      background-color: rgba(102, 126, 234, 0.1);
    }
    .song-list-item.active {
      background-color: rgba(102, 126, 234, 0.2);
      border-left: 3px solid #667eea;
    }
  </style>
</head>
<body>
  <nav class="navbar navbar-expand-lg" style="background-color: rgba(255,255,255,0.9);">
    <div class="container">
      <a class="navbar-brand fw-bold" href="/">
        <i class="bi bi-music-note-list me-2"></i>Music Playlist
      </a>
    </div>
  </nav>

  <main class="container py-4">
    @!section('content')
  </main>

  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"></script>
  @!section('scripts')
</body>
</html>
