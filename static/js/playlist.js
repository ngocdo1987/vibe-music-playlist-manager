/**
 * Playlist Editor - Drag-drop upload and song reordering
 */

let playlistId = null;
let sortable = null;

/**
 * Initialize the playlist editor
 */
function initPlaylistEditor(id) {
    playlistId = id;
    initSortable();
    initDropZone();
    initFileInput();
}

/**
 * Initialize SortableJS for drag-drop reordering
 */
function initSortable() {
    const songList = document.getElementById('songList');
    if (!songList) return;

    sortable = new Sortable(songList, {
        animation: 150,
        handle: '.drag-handle',
        ghostClass: 'bg-primary-subtle',
        onEnd: function () {
            saveSongOrder();
        }
    });
}

/**
 * Save the current song order to the server
 */
function saveSongOrder() {
    const songList = document.getElementById('songList');
    if (!songList) return;

    const songIds = Array.from(songList.querySelectorAll('.song-item'))
        .map(item => parseInt(item.dataset.songId));

    fetch(`/admin/playlist/${playlistId}/order`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify({ song_ids: songIds })
    })
        .then(response => response.json())
        .then(data => {
            if (!data.success) {
                console.error('Failed to save order:', data.error);
            }
        })
        .catch(error => {
            console.error('Error saving order:', error);
        });
}

/**
 * Initialize the drag-drop upload zone
 */
function initDropZone() {
    const dropZone = document.getElementById('dropZone');
    if (!dropZone) return;

    ['dragenter', 'dragover', 'dragleave', 'drop'].forEach(eventName => {
        dropZone.addEventListener(eventName, preventDefaults, false);
    });

    ['dragenter', 'dragover'].forEach(eventName => {
        dropZone.addEventListener(eventName, () => {
            dropZone.classList.add('border-primary', 'bg-primary-subtle');
        }, false);
    });

    ['dragleave', 'drop'].forEach(eventName => {
        dropZone.addEventListener(eventName, () => {
            dropZone.classList.remove('border-primary', 'bg-primary-subtle');
        }, false);
    });

    dropZone.addEventListener('drop', handleDrop, false);
}

function preventDefaults(e) {
    e.preventDefault();
    e.stopPropagation();
}

function handleDrop(e) {
    const files = e.dataTransfer.files;
    if (files.length > 0) {
        uploadFiles(files);
    }
}

/**
 * Initialize the file input button
 */
function initFileInput() {
    const fileInput = document.getElementById('fileInput');
    if (!fileInput) return;

    fileInput.addEventListener('change', function () {
        if (this.files.length > 0) {
            uploadFiles(this.files);
            this.value = ''; // Reset for next selection
        }
    });
}

/**
 * Upload files to the server
 */
function uploadFiles(files) {
    const formData = new FormData();
    formData.append('playlist_id', playlistId);

    let validFiles = 0;
    for (let i = 0; i < files.length; i++) {
        const file = files[i];
        if (file.name.toLowerCase().endsWith('.mp3')) {
            formData.append('files', file);
            validFiles++;
        }
    }

    if (validFiles === 0) {
        showUploadError('Please select MP3 files only');
        return;
    }

    showUploadProgress();

    fetch('/admin/upload', {
        method: 'POST',
        body: formData
    })
        .then(response => response.json())
        .then(data => {
            hideUploadProgress();

            if (data.success && data.songs) {
                data.songs.forEach(song => {
                    if (song.error) {
                        showUploadError(`${song.filename}: ${song.error}`);
                    } else {
                        addSongToList(song);
                    }
                });
            } else {
                showUploadError('Upload failed');
            }
        })
        .catch(error => {
            hideUploadProgress();
            showUploadError('Upload failed: ' + error.message);
        });
}

/**
 * Add a new song to the list
 */
function addSongToList(song) {
    const songList = document.getElementById('songList');
    const emptyState = document.getElementById('emptyState');

    // Remove empty state if present
    if (emptyState) {
        emptyState.remove();
    }

    // Create song list if it doesn't exist
    if (!songList) {
        const container = document.querySelector('.card-body.p-0');
        if (container) {
            const ul = document.createElement('ul');
            ul.className = 'list-group list-group-flush';
            ul.id = 'songList';
            container.appendChild(ul);
            initSortable();
        }
    }

    const li = document.createElement('li');
    li.className = 'list-group-item d-flex justify-content-between align-items-center song-item';
    li.dataset.songId = song.id;
    li.innerHTML = `
        <div class="d-flex align-items-center">
            <i class="bi bi-grip-vertical text-muted me-3 drag-handle" style="cursor: grab;"></i>
            <i class="bi bi-file-earmark-music text-primary me-2"></i>
            <span>${escapeHtml(song.original_name)}</span>
        </div>
        <button type="button" class="btn btn-sm btn-outline-danger" 
                onclick="removeSong(${playlistId}, ${song.id}, this)">
            <i class="bi bi-x-lg"></i>
        </button>
    `;

    document.getElementById('songList').appendChild(li);
}

/**
 * Remove a song from the playlist
 */
function removeSong(playlistId, songId, button) {
    if (!confirm('Remove this song from the playlist?')) return;

    fetch(`/admin/playlist/${playlistId}/song/${songId}/remove`, {
        method: 'POST'
    })
        .then(response => response.json())
        .then(data => {
            if (data.success) {
                const li = button.closest('.song-item');
                li.remove();

                // Show empty state if no songs left
                const songList = document.getElementById('songList');
                if (songList && songList.children.length === 0) {
                    songList.innerHTML = `
                    <div class="text-center py-4" id="emptyState">
                        <i class="bi bi-music-note-list display-4 text-muted"></i>
                        <p class="text-muted mt-2">No songs yet. Upload some MP3 files!</p>
                    </div>
                `;
                }
            }
        })
        .catch(error => {
            console.error('Error removing song:', error);
        });
}

/**
 * Show upload progress
 */
function showUploadProgress() {
    const progress = document.getElementById('uploadProgress');
    if (progress) {
        progress.classList.remove('d-none');
        progress.querySelector('.progress-bar').style.width = '50%';
    }
}

/**
 * Hide upload progress
 */
function hideUploadProgress() {
    const progress = document.getElementById('uploadProgress');
    if (progress) {
        progress.classList.add('d-none');
        progress.querySelector('.progress-bar').style.width = '0%';
    }
}

/**
 * Show upload error
 */
function showUploadError(message) {
    const container = document.getElementById('uploadErrors');
    if (container) {
        const alert = document.createElement('div');
        alert.className = 'alert alert-danger alert-dismissible fade show';
        alert.innerHTML = `
            ${escapeHtml(message)}
            <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
        `;
        container.appendChild(alert);

        // Auto-remove after 5 seconds
        setTimeout(() => alert.remove(), 5000);
    }
}

/**
 * Escape HTML to prevent XSS
 */
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}
