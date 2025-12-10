# Views - Frontend Pages

## Home Page

File: `resources/views/frontend/home.edge`

\`\`\`html
@layout('layouts/frontend')

@section('content')
  <div class="text-center text-white mb-5">
    <h1 class="display-4 fw-bold">Music Playlists</h1>
    <p class="lead">Browse and listen to curated playlists</p>
  </div>

  <div class="row g-4">
    @each(playlist in playlists)
      <div class="col-md-4">
        <a href="/playlist/{{ playlist.slug }}" class="text-decoration-none">
          <div class="card playlist-card h-100">
            <div class="card-body">
              <div class="d-flex align-items-center mb-3">
                <div class="bg-primary bg-opacity-10 rounded-circle p-3 me-3">
                  <i class="bi bi-music-note-list fs-4 text-primary"></i>
                </div>
                <div>
                  <h5 class="card-title mb-0">{{ playlist.name }}</h5>
                  <small class="text-muted">{{ playlist.$extras.songs_count || 0 }} songs</small>
                </div>
              </div>
              @if(playlist.description)
                <p class="card-text text-muted small">{{ playlist.description }}</p>
              @endif
            </div>
            <div class="card-footer bg-transparent border-0">
              <small class="text-primary">
                <i class="bi bi-play-circle me-1"></i>Listen Now
              </small>
            </div>
          </div>
        </a>
      </div>
    @empty
      <div class="col-12">
        <div class="card text-center py-5">
          <div class="card-body">
            <i class="bi bi-music-note-beamed fs-1 text-muted mb-3 d-block"></i>
            <h5>No playlists available</h5>
            <p class="text-muted">Check back later for new music!</p>
          </div>
        </div>
      </div>
    @endeach
  </div>
@endsection
\`\`\`

## Playlist Player Page

File: `resources/views/frontend/playlist.edge`

\`\`\`html
@layout('layouts/frontend')

@section('content')
  <div class="row justify-content-center">
    <div class="col-lg-8">
      <div class="card shadow-lg">
        <div class="card-header bg-primary text-white py-4">
          <div class="d-flex align-items-center">
            <a href="/" class="btn btn-light btn-sm me-3">
              <i class="bi bi-arrow-left"></i>
            </a>
            <div>
              <h2 class="mb-0">{{ playlist.name }}</h2>
              @if(playlist.description)
                <p class="mb-0 opacity-75 small">{{ playlist.description }}</p>
              @endif
            </div>
          </div>
        </div>

        <div class="card-body p-0">
          @if(playlist.songs.length > 0)
            <ul class="list-group list-group-flush" id="songList">
              @each((song, index) in playlist.songs)
                <li class="list-group-item song-list-item d-flex align-items-center py-3" 
                    data-index="{{ index }}" 
                    data-src="/mp3/{{ song.filename }}">
                  <span class="badge bg-secondary me-3">{{ index + 1 }}</span>
                  <div class="flex-grow-1">
                    <h6 class="mb-0">{{ song.title }}</h6>
                    <small class="text-muted">{{ song.originalName }}</small>
                  </div>
                  <button class="btn btn-sm btn-primary play-btn" data-index="{{ index }}">
                    <i class="bi bi-play-fill"></i>
                  </button>
                </li>
              @endeach
            </ul>
          @else
            <div class="text-center py-5">
              <i class="bi bi-music-note-beamed fs-1 text-muted mb-3 d-block"></i>
              <h5>No songs in this playlist</h5>
            </div>
          @endif
        </div>

        @if(playlist.songs.length > 0)
          <div class="card-footer player-container py-3">
            <div class="d-flex align-items-center mb-2">
              <span class="badge bg-primary me-2" id="currentTrackNumber">1</span>
              <span id="currentTrackTitle" class="fw-medium">{{ playlist.songs[0]?.title || 'No track' }}</span>
            </div>
            
            <audio id="audioPlayer" class="w-100 mb-2" controls>
              <source src="/mp3/{{ playlist.songs[0]?.filename || '' }}" type="audio/mpeg">
              Your browser does not support the audio element.
            </audio>

            <div class="d-flex justify-content-center gap-2">
              <button class="btn btn-outline-primary" id="prevBtn">
                <i class="bi bi-skip-backward-fill"></i>
              </button>
              <button class="btn btn-primary px-4" id="playPauseBtn">
                <i class="bi bi-play-fill" id="playPauseIcon"></i>
              </button>
              <button class="btn btn-outline-primary" id="nextBtn">
                <i class="bi bi-skip-forward-fill"></i>
              </button>
              <button class="btn btn-outline-secondary" id="shuffleBtn" title="Shuffle">
                <i class="bi bi-shuffle"></i>
              </button>
              <button class="btn btn-outline-secondary active" id="repeatBtn" title="Repeat">
                <i class="bi bi-repeat"></i>
              </button>
            </div>
          </div>
        @endif
      </div>
    </div>
  </div>
@endsection

@section('scripts')
  <script>
    const audioPlayer = document.getElementById('audioPlayer');
    const playPauseBtn = document.getElementById('playPauseBtn');
    const playPauseIcon = document.getElementById('playPauseIcon');
    const prevBtn = document.getElementById('prevBtn');
    const nextBtn = document.getElementById('nextBtn');
    const shuffleBtn = document.getElementById('shuffleBtn');
    const repeatBtn = document.getElementById('repeatBtn');
    const songItems = document.querySelectorAll('.song-list-item');
    const currentTrackNumber = document.getElementById('currentTrackNumber');
    const currentTrackTitle = document.getElementById('currentTrackTitle');

    let currentIndex = 0;
    let isRepeat = true;
    let isShuffle = false;
    const songs = Array.from(songItems).map(item => ({
      src: item.dataset.src,
      title: item.querySelector('h6').textContent,
      index: parseInt(item.dataset.index)
    }));

    function playSong(index) {
      if (songs.length === 0) return;
      
      currentIndex = index;
      const song = songs[currentIndex];
      
      audioPlayer.src = song.src;
      audioPlayer.play();
      
      // Update UI
      updateActiveTrack();
      playPauseIcon.className = 'bi bi-pause-fill';
    }

    function updateActiveTrack() {
      songItems.forEach(item => item.classList.remove('active'));
      songItems[currentIndex]?.classList.add('active');
      currentTrackNumber.textContent = currentIndex + 1;
      currentTrackTitle.textContent = songs[currentIndex]?.title || '';
    }

    // Play/Pause
    playPauseBtn?.addEventListener('click', () => {
      if (audioPlayer.paused) {
        audioPlayer.play();
        playPauseIcon.className = 'bi bi-pause-fill';
      } else {
        audioPlayer.pause();
        playPauseIcon.className = 'bi bi-play-fill';
      }
    });

    // Previous track
    prevBtn?.addEventListener('click', () => {
      if (audioPlayer.currentTime > 3) {
        audioPlayer.currentTime = 0;
      } else {
        currentIndex = currentIndex > 0 ? currentIndex - 1 : songs.length - 1;
        playSong(currentIndex);
      }
    });

    // Next track
    nextBtn?.addEventListener('click', () => {
      if (isShuffle) {
        currentIndex = Math.floor(Math.random() * songs.length);
      } else {
        currentIndex = currentIndex < songs.length - 1 ? currentIndex + 1 : 0;
      }
      playSong(currentIndex);
    });

    // Shuffle toggle
    shuffleBtn?.addEventListener('click', () => {
      isShuffle = !isShuffle;
      shuffleBtn.classList.toggle('active', isShuffle);
    });

    // Repeat toggle
    repeatBtn?.addEventListener('click', () => {
      isRepeat = !isRepeat;
      repeatBtn.classList.toggle('active', isRepeat);
    });

    // Song ended - play next
    audioPlayer?.addEventListener('ended', () => {
      if (isRepeat || currentIndex < songs.length - 1) {
        if (isShuffle) {
          currentIndex = Math.floor(Math.random() * songs.length);
        } else {
          currentIndex = currentIndex < songs.length - 1 ? currentIndex + 1 : 0;
        }
        playSong(currentIndex);
      } else {
        playPauseIcon.className = 'bi bi-play-fill';
      }
    });

    // Click on song item to play
    songItems.forEach(item => {
      item.addEventListener('click', (e) => {
        if (!e.target.closest('.play-btn')) {
          playSong(parseInt(item.dataset.index));
        }
      });
    });

    // Play button on each song
    document.querySelectorAll('.play-btn').forEach(btn => {
      btn.addEventListener('click', () => {
        playSong(parseInt(btn.dataset.index));
      });
    });

    // Update play/pause icon on audio events
    audioPlayer?.addEventListener('play', () => {
      playPauseIcon.className = 'bi bi-pause-fill';
    });

    audioPlayer?.addEventListener('pause', () => {
      playPauseIcon.className = 'bi bi-play-fill';
    });

    // Initialize
    updateActiveTrack();
  </script>
@endsection
