// Admin functionality - Song reordering with drag and drop
(function () {
    'use strict';

    // Initialize sortable list
    function initSortable() {
        const sortableList = document.getElementById('sortable-songs');
        if (!sortableList || typeof Sortable === 'undefined') return;

        const playlistId = sortableList.dataset.playlistId;

        new Sortable(sortableList, {
            handle: '.drag-handle',
            animation: 150,
            ghostClass: 'sortable-ghost',
            chosenClass: 'sortable-chosen',
            onEnd: function (evt) {
                saveSongOrder(playlistId);
            }
        });
    }

    // Save song order to server
    function saveSongOrder(playlistId) {
        const sortableList = document.getElementById('sortable-songs');
        if (!sortableList) return;

        const items = sortableList.querySelectorAll('.sortable-item');
        const songIds = Array.from(items).map(item => item.dataset.songId);

        fetch('/admin/playlist/' + playlistId + '/reorder', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ order: songIds })
        })
            .then(response => response.json())
            .then(data => {
                if (data.status === 'ok') {
                    showToast('Order saved successfully', 'success');
                } else {
                    showToast('Failed to save order', 'error');
                }
            })
            .catch(err => {
                console.error('Error saving order:', err);
                showToast('Error saving order', 'error');
            });
    }

    // Show toast notification
    function showToast(message, type) {
        // Create toast element
        const toast = document.createElement('div');
        toast.className = 'toast align-items-center text-bg-' + (type === 'success' ? 'success' : 'danger');
        toast.setAttribute('role', 'alert');
        toast.innerHTML = `
            <div class="d-flex">
                <div class="toast-body">${message}</div>
                <button type="button" class="btn-close btn-close-white me-2 m-auto" data-bs-dismiss="toast"></button>
            </div>
        `;

        // Create container if it doesn't exist
        let container = document.querySelector('.toast-container');
        if (!container) {
            container = document.createElement('div');
            container.className = 'toast-container position-fixed bottom-0 end-0 p-3';
            document.body.appendChild(container);
        }

        container.appendChild(toast);

        const bsToast = new bootstrap.Toast(toast);
        bsToast.show();

        // Remove after hidden
        toast.addEventListener('hidden.bs.toast', () => toast.remove());
    }

    // Initialize file upload with validation
    function initFileUpload() {
        const uploadForm = document.getElementById('uploadForm');
        if (!uploadForm) return;

        uploadForm.addEventListener('submit', function (e) {
            const fileInput = uploadForm.querySelector('input[type="file"]');
            const files = fileInput.files;

            // Validate file types
            for (let i = 0; i < files.length; i++) {
                const file = files[i];
                if (!file.name.toLowerCase().endsWith('.mp3')) {
                    e.preventDefault();
                    alert('Only MP3 files are allowed. Invalid file: ' + file.name);
                    return false;
                }
            }
        });
    }

    // Run on DOM ready
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', function () {
            initSortable();
            initFileUpload();
        });
    } else {
        initSortable();
        initFileUpload();
    }
})();
