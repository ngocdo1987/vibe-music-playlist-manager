// Music Player functionality
(function () {
    'use strict';

    let currentIndex = -1;
    let playlist = [];
    let audioPlayer;
    let isPlaying = false;

    // Initialize player
    function initPlayer() {
        audioPlayer = document.getElementById('audioPlayer');
        if (!audioPlayer) return;

        // Build playlist from DOM
        const items = document.querySelectorAll('.playlist-item');
        items.forEach((item, index) => {
            playlist.push({
                index: index,
                src: item.dataset.src,
                title: item.querySelector('.song-title')?.textContent || 'Unknown'
            });

            // Add click handler
            item.addEventListener('click', () => playSong(index));
        });

        // Player controls
        const playBtn = document.getElementById('playBtn');
        const prevBtn = document.getElementById('prevBtn');
        const nextBtn = document.getElementById('nextBtn');

        if (playBtn) playBtn.addEventListener('click', togglePlay);
        if (prevBtn) prevBtn.addEventListener('click', playPrevious);
        if (nextBtn) nextBtn.addEventListener('click', playNext);

        // Audio events
        audioPlayer.addEventListener('ended', playNext);
        audioPlayer.addEventListener('play', () => updatePlayButton(true));
        audioPlayer.addEventListener('pause', () => updatePlayButton(false));

        // Auto-play first song if available
        if (playlist.length > 0) {
            loadSong(0);
        }
    }

    // Load a song without playing
    function loadSong(index) {
        if (index < 0 || index >= playlist.length) return;

        currentIndex = index;
        const song = playlist[index];

        audioPlayer.src = song.src;
        updateCurrentTrack(song.title);
        updateActiveItem(index);
    }

    // Play a specific song
    function playSong(index) {
        if (index < 0 || index >= playlist.length) return;

        loadSong(index);
        audioPlayer.play();
        isPlaying = true;
    }

    // Toggle play/pause
    function togglePlay() {
        if (!audioPlayer.src) {
            if (playlist.length > 0) {
                playSong(0);
            }
            return;
        }

        if (audioPlayer.paused) {
            audioPlayer.play();
        } else {
            audioPlayer.pause();
        }
    }

    // Play previous song
    function playPrevious() {
        if (currentIndex > 0) {
            playSong(currentIndex - 1);
        } else if (playlist.length > 0) {
            playSong(playlist.length - 1);
        }
    }

    // Play next song
    function playNext() {
        if (currentIndex < playlist.length - 1) {
            playSong(currentIndex + 1);
        } else if (playlist.length > 0) {
            playSong(0);
        }
    }

    // Update play button icon
    function updatePlayButton(playing) {
        const playBtn = document.getElementById('playBtn');
        if (playBtn) {
            const icon = playBtn.querySelector('i');
            if (icon) {
                icon.className = playing ? 'bi bi-pause-fill' : 'bi bi-play-fill';
            }
        }
        isPlaying = playing;
    }

    // Update current track display
    function updateCurrentTrack(title) {
        const trackDisplay = document.getElementById('currentTrack');
        if (trackDisplay) {
            trackDisplay.textContent = title;
        }
    }

    // Update active item in playlist
    function updateActiveItem(index) {
        const items = document.querySelectorAll('.playlist-item');
        items.forEach((item, i) => {
            if (i === index) {
                item.classList.add('active');
            } else {
                item.classList.remove('active');
            }
        });
    }

    // Run on DOM ready
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initPlayer);
    } else {
        initPlayer();
    }
})();
