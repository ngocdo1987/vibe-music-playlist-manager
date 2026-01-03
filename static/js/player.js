/**
 * Audio Player - Public playlist player functionality
 */

(function () {
    'use strict';

    const audio = document.getElementById('audioPlayer');
    const songList = document.getElementById('playlistSongs');
    const currentTitle = document.getElementById('currentSongTitle');
    const discIcon = document.getElementById('discIcon');

    let currentIndex = -1;
    let songs = [];

    // Initialize on DOM ready
    document.addEventListener('DOMContentLoaded', init);

    function init() {
        if (!audio || !songList) return;

        // Build songs array from DOM
        songs = Array.from(songList.querySelectorAll('.song-row')).map(row => ({
            url: row.dataset.url,
            title: row.dataset.title,
            element: row
        }));

        // Add click handlers to song rows
        songs.forEach((song, index) => {
            song.element.addEventListener('click', () => playSong(index));
        });

        // Audio event handlers
        audio.addEventListener('ended', playNext);
        audio.addEventListener('play', onPlay);
        audio.addEventListener('pause', onPause);
        audio.addEventListener('error', onError);

        // Play first song if available (optional - uncomment to autoplay)
        // if (songs.length > 0) playSong(0);
    }

    /**
     * Play a song by index
     */
    function playSong(index) {
        if (index < 0 || index >= songs.length) return;

        currentIndex = index;
        const song = songs[index];

        // Update audio source
        audio.src = song.url;
        audio.play().catch(e => console.log('Playback prevented:', e));

        // Update title
        if (currentTitle) {
            currentTitle.textContent = song.title;
        }

        // Update visual state
        updateActiveState();
    }

    /**
     * Play the next song
     */
    function playNext() {
        if (currentIndex < songs.length - 1) {
            playSong(currentIndex + 1);
        } else {
            // End of playlist
            onPause();
        }
    }

    /**
     * Update which song appears active in the list
     */
    function updateActiveState() {
        songs.forEach((song, index) => {
            const row = song.element;
            const playIcon = row.querySelector('.play-icon');
            const indicator = row.querySelector('.playing-indicator');

            if (index === currentIndex) {
                row.classList.add('active', 'bg-primary-subtle');
                playIcon.classList.remove('bi-play-circle');
                playIcon.classList.add('bi-pause-circle');
                indicator.classList.remove('d-none');
            } else {
                row.classList.remove('active', 'bg-primary-subtle');
                playIcon.classList.remove('bi-pause-circle');
                playIcon.classList.add('bi-play-circle');
                indicator.classList.add('d-none');
            }
        });
    }

    /**
     * Handle play event
     */
    function onPlay() {
        if (discIcon) {
            discIcon.classList.add('spinning');
        }
    }

    /**
     * Handle pause event
     */
    function onPause() {
        if (discIcon) {
            discIcon.classList.remove('spinning');
        }
    }

    /**
     * Handle audio error
     */
    function onError(e) {
        console.error('Audio error:', e);
        if (currentTitle) {
            currentTitle.textContent = 'Error loading audio';
        }
    }
})();
