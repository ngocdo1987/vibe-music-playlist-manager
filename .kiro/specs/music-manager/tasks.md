# Implementation Plan

- [x] 1. Set up project structure and dependencies
  - Create ASDF system definition (`music-manager.asd`)
  - Create package definition (`src/package.lisp`)
  - Create `.env.example` with required variables
  - _Requirements: 9.3, 9.4_

- [x] 2. Implement configuration module
  - [x] 2.1 Create config loader (`src/config.lisp`)
    - Load .env file using cl-dotenv
    - Define `get-config` function for ADMIN_USER, ADMIN_PASS, DB_PATH
    - Exit with error if required variables missing
    - _Requirements: 2.5_

- [x] 3. Implement database layer
  - [x] 3.1 Create database module (`src/db.lisp`)
    - Define connection function using cl-dbi
    - Create schema initialization with all three tables
    - _Requirements: 1.1, 1.2, 1.3, 1.4_
  - [x] 3.2 Create models module (`src/models.lisp`)
    - Implement playlist CRUD functions
    - Implement song CRUD functions
    - Implement playlist_songs operations with ordering
    - _Requirements: 1.2, 1.3, 1.4_

- [x] 4. Implement authentication
  - [x] 4.1 Create auth module (`src/auth.lisp`)
    - Implement `authenticate` function comparing with .env credentials
    - Implement session-based `login-user` and `logout-user`
    - Create `require-auth` middleware function
    - _Requirements: 2.1, 2.2, 2.3, 2.4_

- [x] 5. Implement file upload handling
  - [x] 5.1 Create upload module (`src/upload.lisp`)
    - Implement `validate-mp3` with extension and magic byte checks
    - Implement `save-uploaded-file` with unique filename generation
    - Create mp3 directory if not exists
    - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5, 4.6_

- [x] 6. Implement routes and controllers
  - [x] 6.1 Create routes module (`src/routes.lisp`)
    - Define public routes (home, playlist view)
    - Define auth routes (login, logout)
    - Define admin routes (dashboard, playlist CRUD)
    - Define upload and order update endpoints
    - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 7.1, 7.2, 7.3, 7.4, 7.5, 7.6_

- [x] 7. Create templates
  - [x] 7.1 Create base template (`templates/base.html`)
    - Bootstrap 5 layout with theme toggle
    - Navigation for admin area
    - _Requirements: 6.1, 6.2, 6.3, 8.1, 8.2_
  - [x] 7.2 Create admin templates
    - Login page (`templates/admin/login.html`)
    - Dashboard (`templates/admin/dashboard.html`)
    - Playlist form (`templates/admin/playlist-form.html`)
    - _Requirements: 3.1, 3.2, 3.4, 8.3_
  - [x] 7.3 Create public templates
    - Playlist player (`templates/public/playlist.html`)
    - 404 error page (`templates/public/404.html`)
    - _Requirements: 7.2, 7.3, 7.6, 8.4_

- [x] 8. Implement frontend JavaScript
  - [x] 8.1 Create theme toggle (`static/js/theme.js`)
    - Switch Bootstrap theme via data-bs-theme
    - Save preference to localStorage
    - _Requirements: 6.1, 6.2, 6.3, 6.4_
  - [x] 8.2 Create drag-drop sorting (`static/js/playlist.js`)
    - Initialize SortableJS on song list
    - Send order update via AJAX
    - _Requirements: 5.1, 5.2, 5.3, 5.4_
  - [x] 8.3 Create audio player logic (`static/js/player.js`)
    - Handle song click to play
    - Auto-advance to next song on end
    - Highlight current song
    - _Requirements: 7.4, 7.5_

- [x] 9. Create main entry point
  - [x] 9.1 Create main module (`src/main.lisp`)
    - Initialize config, database, and Hunchentoot
    - Start server function
    - Static file serving for /static and /mp3
    - _Requirements: 1.1, 9.3_

- [x] 10. Create custom CSS
  - [x] 10.1 Create styles (`static/css/custom.css`)
    - Modern, clean styling
    - Player UI enhancements
    - _Requirements: 8.1, 8.2, 8.3, 8.4_

- [x] 11. Write deployment documentation
  - [x] 11.1 Create README.md
    - Prerequisites (SBCL, Quicklisp, SQLite3)
    - Installation steps
    - Running the application
    - Production deployment with systemd
    - _Requirements: 9.4_

- [ ] 12. Testing and verification
  - [ ] 12.1 Write unit tests for validate-mp3 function
    - Test valid MP3 with ID3 header
    - Test valid MP3 with frame sync
    - Test invalid file rejection
    - _Requirements: 4.2, 4.3, 4.4_
  - [ ] 12.2 Test full application flow
    - Verify login/logout
    - Verify playlist CRUD
    - Verify upload and ordering
    - Verify public playback
    - _Requirements: All_
