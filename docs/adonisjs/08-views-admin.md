# Views - Admin Pages

## Login Page

File: `resources/views/admin/login.edge`

\`\`\`html
<!DOCTYPE html>
<html lang="en" data-bs-theme="light">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Admin Login - Music Playlist</title>
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css" rel="stylesheet">
</head>
<body class="d-flex align-items-center min-vh-100 bg-body-tertiary">
  <div class="container">
    <div class="row justify-content-center">
      <div class="col-md-4">
        <div class="card shadow">
          <div class="card-body p-5">
            <div class="text-center mb-4">
              <i class="bi bi-music-note-list fs-1 text-primary"></i>
              <h3 class="mt-2">Admin Login</h3>
            </div>

            @if(flashMessages.has('error'))
              <div class="alert alert-danger">{{ flashMessages.get('error') }}</div>
            @endif

            @if(flashMessages.has('success'))
              <div class="alert alert-success">{{ flashMessages.get('success') }}</div>
            @endif

            <form action="/admin/login" method="POST">
              <div class="mb-3">
                <label for="username" class="form-label">Username</label>
                <input type="text" class="form-control" id="username" name="username" required autofocus>
              </div>
              <div class="mb-3">
                <label for="password" class="form-label">Password</label>
                <input type="password" class="form-control" id="password" name="password" required>
              </div>
              <button type="submit" class="btn btn-primary w-100">
                <i class="bi bi-box-arrow-in-right me-2"></i>Login
              </button>
            </form>
          </div>
        </div>
      </div>
    </div>
  </div>
  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>
\`\`\`

## Dashboard Page

File: `resources/views/admin/dashboard.edge`

\`\`\`html
@layout('layouts/admin')

@section('content')
  <div class="d-flex justify-content-between align-items-center mb-4">
    <h1>Dashboard</h1>
    <a href="/admin/playlists/create" class="btn btn-primary">
      <i class="bi bi-plus-lg me-2"></i>Create Playlist
    </a>
  </div>

  <div class="row g-4">
    <div class="col-md-4">
      <div class="card bg-primary text-white">
        <div class="card-body">
          <div class="d-flex justify-content-between align-items-center">
            <div>
              <h6 class="card-subtitle mb-2 opacity-75">Total Playlists</h6>
              <h2 class="card-title mb-0">{{ playlists.length }}</h2>
            </div>
            <i class="bi bi-collection fs-1 opacity-50"></i>
          </div>
        </div>
      </div>
    </div>
    <div class="col-md-4">
      <div class="card bg-success text-white">
        <div class="card-body">
          <div class="d-flex justify-content-between align-items-center">
            <div>
              <h6 class="card-subtitle mb-2 opacity-75">Total Songs</h6>
              <h2 class="card-title mb-0">{{ playlists.reduce((sum, p) => sum + Number(p.$extras.songs_count || 0), 0) }}</h2>
            </div>
            <i class="bi bi-music-note fs-1 opacity-50"></i>
          </div>
        </div>
      </div>
    </div>
  </div>

  <h4 class="mt-5 mb-3">Recent Playlists</h4>
  <div class="card">
    <div class="table-responsive">
      <table class="table table-hover mb-0">
        <thead>
          <tr>
            <th>Name</th>
            <th>Songs</th>
            <th>Created</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          @each(playlist in playlists.slice(0, 5))
            <tr>
              <td>
                <a href="/playlist/{{ playlist.slug }}" target="_blank" class="text-decoration-none">
                  {{ playlist.name }}
                  <i class="bi bi-box-arrow-up-right ms-1 small"></i>
                </a>
              </td>
              <td>{{ playlist.$extras.songs_count || 0 }}</td>
              <td>{{ playlist.createdAt.toFormat('dd MMM yyyy') }}</td>
              <td>
                <a href="/admin/playlists/{{ playlist.id }}/edit" class="btn btn-sm btn-outline-primary">
                  <i class="bi bi-pencil"></i>
                </a>
              </td>
            </tr>
          @endeach
        </tbody>
      </table>
    </div>
  </div>
@endsection
\`\`\`

## Playlists Index Page

File: `resources/views/admin/playlists/index.edge`

\`\`\`html
@layout('layouts/admin')

@section('content')
  <div class="d-flex justify-content-between align-items-center mb-4">
    <h1>Playlists</h1>
    <a href="/admin/playlists/create" class="btn btn-primary">
      <i class="bi bi-plus-lg me-2"></i>Create Playlist
    </a>
  </div>

  <div class="card">
    <div class="table-responsive">
      <table class="table table-hover mb-0">
        <thead>
          <tr>
            <th>#</th>
            <th>Name</th>
            <th>Slug</th>
            <th>Songs</th>
            <th>Created</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          @each(playlist in playlists)
            <tr>
              <td>{{ playlist.id }}</td>
              <td>
                <a href="/playlist/{{ playlist.slug }}" target="_blank" class="text-decoration-none fw-medium">
                  {{ playlist.name }}
                  <i class="bi bi-box-arrow-up-right ms-1 small"></i>
                </a>
              </td>
              <td><code>{{ playlist.slug }}</code></td>
              <td>{{ playlist.$extras.songs_count || 0 }}</td>
              <td>{{ playlist.createdAt.toFormat('dd MMM yyyy') }}</td>
              <td>
                <div class="btn-group btn-group-sm">
                  <a href="/admin/playlists/{{ playlist.id }}/edit" class="btn btn-outline-primary">
                    <i class="bi bi-pencil"></i>
                  </a>
                  <button type="button" class="btn btn-outline-danger" 
                    onclick="if(confirm('Are you sure you want to delete this playlist?')) document.getElementById('delete-{{ playlist.id }}').submit()">
                    <i class="bi bi-trash"></i>
                  </button>
                </div>
                <form id="delete-{{ playlist.id }}" action="/admin/playlists/{{ playlist.id }}?_method=DELETE" method="POST" class="d-none">
                </form>
              </td>
            </tr>
          @empty
            <tr>
              <td colspan="6" class="text-center py-4 text-muted">
                <i class="bi bi-collection fs-1 d-block mb-2"></i>
                No playlists yet. Create your first one!
              </td>
            </tr>
          @endeach
        </tbody>
      </table>
    </div>
  </div>
@endsection
\`\`\`

## Create Playlist Page

File: `resources/views/admin/playlists/create.edge`

\`\`\`html
@layout('layouts/admin')

@section('content')
  <div class="d-flex align-items-center mb-4">
    <a href="/admin/playlists" class="btn btn-outline-secondary me-3">
      <i class="bi bi-arrow-left"></i>
    </a>
    <h1 class="mb-0">Create Playlist</h1>
  </div>

  <div class="card">
    <div class="card-body">
      <form action="/admin/playlists" method="POST" enctype="multipart/form-data" id="playlistForm">
        <div class="mb-3">
          <label for="name" class="form-label">Playlist Name *</label>
          <input type="text" class="form-control" id="name" name="name" required>
        </div>

        <div class="mb-3">
          <label for="description" class="form-label">Description</label>
          <textarea class="form-control" id="description" name="description" rows="3"></textarea>
        </div>

        <div class="mb-4">
          <label class="form-label">Upload Songs (MP3 only)</label>
          <div class="drop-zone" id="dropZone">
            <i class="bi bi-cloud-upload fs-1 text-muted"></i>
            <p class="mb-2">Drag and drop MP3 files here</p>
            <p class="text-muted small mb-0">or click to browse</p>
            <input type="file" name="songs" id="fileInput" multiple accept=".mp3,audio/mpeg" class="d-none">
          </div>
          <div id="fileList" class="mt-3"></div>
        </div>

        <div class="d-flex gap-2">
          <button type="submit" class="btn btn-primary">
            <i class="bi bi-check-lg me-2"></i>Create Playlist
          </button>
          <a href="/admin/playlists" class="btn btn-outline-secondary">Cancel</a>
        </div>
      </form>
    </div>
  </div>
@endsection

@section('scripts')
  <script>
    const dropZone = document.getElementById('dropZone');
    const fileInput = document.getElementById('fileInput');
    const fileList = document.getElementById('fileList');
    let selectedFiles = [];

    // Click to open file dialog
    dropZone.addEventListener('click', () => fileInput.click());

    // Drag and drop handlers
    dropZone.addEventListener('dragover', (e) => {
      e.preventDefault();
      dropZone.classList.add('drag-over');
    });

    dropZone.addEventListener('dragleave', () => {
      dropZone.classList.remove('drag-over');
    });

    dropZone.addEventListener('drop', (e) => {
      e.preventDefault();
      dropZone.classList.remove('drag-over');
      handleFiles(e.dataTransfer.files);
    });

    fileInput.addEventListener('change', (e) => {
      handleFiles(e.target.files);
    });

    function handleFiles(files) {
      for (const file of files) {
        if (file.type === 'audio/mpeg' || file.name.endsWith('.mp3')) {
          if (!selectedFiles.find(f => f.name === file.name)) {
            selectedFiles.push(file);
          }
        } else {
          alert(`${file.name} is not a valid MP3 file`);
        }
      }
      updateFileList();
      updateFormData();
    }

    function updateFileList() {
      fileList.innerHTML = selectedFiles.map((file, index) => `
        <div class="d-flex align-items-center justify-content-between p-2 border rounded mb-2">
          <div>
            <i class="bi bi-music-note me-2"></i>
            <span>${file.name}</span>
            <small class="text-muted ms-2">(${(file.size / 1024 / 1024).toFixed(2)} MB)</small>
          </div>
          <button type="button" class="btn btn-sm btn-outline-danger" onclick="removeFile(${index})">
            <i class="bi bi-x"></i>
          </button>
        </div>
      `).join('');
    }

    function removeFile(index) {
      selectedFiles.splice(index, 1);
      updateFileList();
      updateFormData();
    }

    function updateFormData() {
      const dataTransfer = new DataTransfer();
      selectedFiles.forEach(file => dataTransfer.items.add(file));
      fileInput.files = dataTransfer.files;
    }
  </script>
@endsection
\`\`\`

## Edit Playlist Page

File: `resources/views/admin/playlists/edit.edge`

\`\`\`html
@layout('layouts/admin')

@section('content')
  <div class="d-flex align-items-center mb-4">
    <a href="/admin/playlists" class="btn btn-outline-secondary me-3">
      <i class="bi bi-arrow-left"></i>
    </a>
    <h1 class="mb-0">Edit Playlist</h1>
  </div>

  <div class="row">
    <div class="col-lg-8">
      <div class="card mb-4">
        <div class="card-header">
          <h5 class="mb-0">Playlist Details</h5>
        </div>
        <div class="card-body">
          <form action="/admin/playlists/{{ playlist.id }}?_method=PUT" method="POST" enctype="multipart/form-data" id="playlistForm">
            <input type="hidden" name="songOrder" id="songOrder">
            
            <div class="mb-3">
              <label for="name" class="form-label">Playlist Name *</label>
              <input type="text" class="form-control" id="name" name="name" value="{{ playlist.name }}" required>
            </div>

            <div class="mb-3">
              <label for="description" class="form-label">Description</label>
              <textarea class="form-control" id="description" name="description" rows="3">{{ playlist.description || '' }}</textarea>
            </div>

            <div class="mb-4">
              <label class="form-label">Add More Songs (MP3 only)</label>
              <div class="drop-zone" id="dropZone">
                <i class="bi bi-cloud-upload fs-1 text-muted"></i>
                <p class="mb-2">Drag and drop MP3 files here</p>
                <p class="text-muted small mb-0">or click to browse</p>
                <input type="file" name="songs" id="fileInput" multiple accept=".mp3,audio/mpeg" class="d-none">
              </div>
              <div id="newFileList" class="mt-3"></div>
            </div>

            <div class="d-flex gap-2">
              <button type="submit" class="btn btn-primary">
                <i class="bi bi-check-lg me-2"></i>Save Changes
              </button>
              <a href="/admin/playlists" class="btn btn-outline-secondary">Cancel</a>
            </div>
          </form>
        </div>
      </div>
    </div>

    <div class="col-lg-4">
      <div class="card">
        <div class="card-header d-flex justify-content-between align-items-center">
          <h5 class="mb-0">Songs ({{ playlist.songs.length }})</h5>
          <small class="text-muted">Drag to reorder</small>
        </div>
        <div class="card-body p-0">
          <ul class="list-group list-group-flush" id="songList">
            @each(song in playlist.songs)
              <li class="list-group-item song-item d-flex align-items-center" data-id="{{ song.id }}">
                <i class="bi bi-grip-vertical me-2 text-muted"></i>
                <div class="flex-grow-1">
                  <input type="text" class="form-control form-control-sm border-0 p-0 song-title" 
                    value="{{ song.title }}" 
                    data-id="{{ song.id }}"
                    style="background: transparent;">
                  <small class="text-muted">{{ song.originalName }}</small>
                </div>
                <button type="button" class="btn btn-sm btn-outline-danger ms-2"
                  onclick="if(confirm('Delete this song?')) document.getElementById('delete-song-{{ song.id }}').submit()">
                  <i class="bi bi-trash"></i>
                </button>
                <form id="delete-song-{{ song.id }}" action="/admin/songs/{{ song.id }}?_method=DELETE" method="POST" class="d-none"></form>
              </li>
            @empty
              <li class="list-group-item text-center text-muted py-4">
                <i class="bi bi-music-note-beamed fs-1 d-block mb-2"></i>
                No songs yet
              </li>
            @endeach
          </ul>
        </div>
      </div>
    </div>
  </div>
@endsection

@section('scripts')
  <script src="https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"></script>
  <script>
    // Drag and drop for reordering songs
    const songList = document.getElementById('songList');
    if (songList.children.length > 0 && !songList.querySelector('.text-muted.py-4')) {
      new Sortable(songList, {
        animation: 150,
        ghostClass: 'dragging',
        handle: '.bi-grip-vertical',
        onEnd: updateSongOrder
      });
    }

    function updateSongOrder() {
      const items = songList.querySelectorAll('.song-item');
      const order = Array.from(items).map(item => item.dataset.id);
      document.getElementById('songOrder').value = JSON.stringify(order);
    }

    // Initialize song order
    updateSongOrder();

    // Update song title on blur
    document.querySelectorAll('.song-title').forEach(input => {
      input.addEventListener('blur', async function() {
        const id = this.dataset.id;
        const title = this.value;
        
        await fetch(`/admin/songs/${id}/title?_method=PUT`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ title })
        });
      });
    });

    // File upload handling
    const dropZone = document.getElementById('dropZone');
    const fileInput = document.getElementById('fileInput');
    const newFileList = document.getElementById('newFileList');
    let selectedFiles = [];

    dropZone.addEventListener('click', () => fileInput.click());

    dropZone.addEventListener('dragover', (e) => {
      e.preventDefault();
      dropZone.classList.add('drag-over');
    });

    dropZone.addEventListener('dragleave', () => {
      dropZone.classList.remove('drag-over');
    });

    dropZone.addEventListener('drop', (e) => {
      e.preventDefault();
      dropZone.classList.remove('drag-over');
      handleFiles(e.dataTransfer.files);
    });

    fileInput.addEventListener('change', (e) => {
      handleFiles(e.target.files);
    });

    function handleFiles(files) {
      for (const file of files) {
        if (file.type === 'audio/mpeg' || file.name.endsWith('.mp3')) {
          if (!selectedFiles.find(f => f.name === file.name)) {
            selectedFiles.push(file);
          }
        } else {
          alert(`${file.name} is not a valid MP3 file`);
        }
      }
      updateFileList();
      updateFormData();
    }

    function updateFileList() {
      newFileList.innerHTML = selectedFiles.map((file, index) => `
        <div class="d-flex align-items-center justify-content-between p-2 border rounded mb-2">
          <div>
            <i class="bi bi-music-note me-2"></i>
            <span>${file.name}</span>
            <small class="text-muted ms-2">(${(file.size / 1024 / 1024).toFixed(2)} MB)</small>
          </div>
          <button type="button" class="btn btn-sm btn-outline-danger" onclick="removeFile(${index})">
            <i class="bi bi-x"></i>
          </button>
        </div>
      `).join('');
    }

    function removeFile(index) {
      selectedFiles.splice(index, 1);
      updateFileList();
      updateFormData();
    }

    function updateFormData() {
      const dataTransfer = new DataTransfer();
      selectedFiles.forEach(file => dataTransfer.items.add(file));
      fileInput.files = dataTransfer.files;
    }
  </script>
@endsection
