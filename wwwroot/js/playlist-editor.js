// Playlist editor with drag-and-drop functionality
let playlistId = null;

function initPlaylistEditor(id) {
    playlistId = id;

    const songList = document.getElementById('songList');
    if (!songList) return;

    const items = songList.querySelectorAll('.list-group-item');

    items.forEach(item => {
        item.addEventListener('dragstart', handleDragStart);
        item.addEventListener('dragover', handleDragOver);
        item.addEventListener('drop', handleDrop);
        item.addEventListener('dragend', handleDragEnd);
    });
}

let draggedItem = null;

function handleDragStart(e) {
    draggedItem = this;
    this.style.opacity = '0.4';
    e.dataTransfer.effectAllowed = 'move';
    e.dataTransfer.setData('text/html', this.innerHTML);
}

function handleDragOver(e) {
    if (e.preventDefault) {
        e.preventDefault();
    }
    e.dataTransfer.dropEffect = 'move';

    const targetItem = e.target.closest('.list-group-item');
    if (targetItem && targetItem !== draggedItem) {
        targetItem.style.borderTop = '2px solid #007bff';
    }

    return false;
}

function handleDrop(e) {
    if (e.stopPropagation) {
        e.stopPropagation();
    }

    const targetItem = e.target.closest('.list-group-item');
    if (targetItem && draggedItem !== targetItem) {
        const songList = document.getElementById('songList');
        const allItems = Array.from(songList.querySelectorAll('.list-group-item'));
        const draggedIndex = allItems.indexOf(draggedItem);
        const targetIndex = allItems.indexOf(targetItem);

        if (draggedIndex < targetIndex) {
            targetItem.parentNode.insertBefore(draggedItem, targetItem.nextSibling);
        } else {
            targetItem.parentNode.insertBefore(draggedItem, targetItem);
        }

        // Update order on server
        updateSongOrder();
    }

    targetItem.style.borderTop = '';

    return false;
}

function handleDragEnd(e) {
    this.style.opacity = '1';

    const items = document.querySelectorAll('.list-group-item');
    items.forEach(item => {
        item.style.borderTop = '';
    });
}

function updateSongOrder() {
    const songList = document.getElementById('songList');
    const items = songList.querySelectorAll('.list-group-item');
    const songIds = Array.from(items).map(item => parseInt(item.dataset.songId));

    // Send update to server via AJAX
    fetch(`/playlistadmin/updateorder?playlistId=${playlistId}`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(songIds)
    })
        .then(response => {
            if (response.ok) {
                console.log('Order updated successfully');
            } else {
                console.error('Failed to update order');
            }
        })
        .catch(error => {
            console.error('Error updating order:', error);
        });
}
