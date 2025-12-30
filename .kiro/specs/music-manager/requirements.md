# Requirements Document

## Introduction

A pure Zig web application for managing MP3 playlists. Uses Zig's standard library HTTP server and SQLite for data storage. Features a public frontend for music playback and a protected admin backend for content management. Bootstrap 5 provides the UI styling.

## Requirements

### Requirement 1: Database Schema

**User Story:** As a developer, I want a well-structured SQLite database, so that I can efficiently store playlists and songs.

#### Acceptance Criteria

1. WHEN the application starts THEN the system SHALL create SQLite tables for playlists, songs, and playlist_songs (junction table)
2. The playlist table SHALL contain id, name, slug, created_at, updated_at columns
3. The song table SHALL contain id, title, filename, file_path, created_at columns
4. The playlist_songs junction table SHALL contain playlist_id, song_id, position columns

---

### Requirement 2: Admin Authentication

**User Story:** As an admin, I want to log in with username and password, so that I can securely access the backend.

#### Acceptance Criteria

1. WHEN a user accesses any /admin/* URL THEN the system SHALL redirect to login IF not authenticated
2. WHEN a user submits valid credentials THEN the system SHALL create a session and redirect to dashboard
3. WHEN a user submits invalid credentials THEN the system SHALL display an error message
4. WHEN a user clicks logout THEN the system SHALL destroy the session
5. The admin credentials SHALL be stored in a .env file (ADMIN_USERNAME, ADMIN_PASSWORD)

---

### Requirement 3: Playlist Management

**User Story:** As an admin, I want to create, edit, and delete playlists, so that I can organize music collections.

#### Acceptance Criteria

1. WHEN an admin clicks "Create Playlist" THEN the system SHALL display a form with name field and MP3 upload
2. WHEN an admin submits a valid playlist form THEN the system SHALL save to database and redirect to list
3. WHEN an admin clicks "Edit" THEN the system SHALL display the edit form with existing data
4. WHEN an admin clicks "Delete" THEN the system SHALL confirm and delete the playlist
5. WHEN an admin views playlist list THEN the system SHALL display all playlists with actions

---

### Requirement 4: MP3 File Upload & Validation

**User Story:** As an admin, I want to upload MP3 files, so that I can add songs to playlists.

#### Acceptance Criteria

1. WHEN an admin uploads files THEN the system SHALL accept only valid .mp3 files
2. IF an uploaded file is not a valid MP3 THEN the system SHALL reject it with error message
3. WHEN a valid MP3 is uploaded THEN the system SHALL save it to the "mp3" folder
4. WHEN a valid MP3 is uploaded THEN the system SHALL extract filename as song title
5. The system SHALL support multiple file uploads in a single operation

---

### Requirement 5: Drag-and-Drop Song Ordering

**User Story:** As an admin, I want to drag and drop songs to reorder them within a playlist.

#### Acceptance Criteria

1. WHEN viewing playlist edit form THEN the system SHALL display songs in a draggable list
2. WHEN an admin drags a song THEN the system SHALL update visual order immediately
3. WHEN an admin saves the playlist THEN the system SHALL persist the new order
4. WHEN songs are reordered THEN the system SHALL update position in database

---

### Requirement 6: Bootstrap Theme Switching

**User Story:** As an admin, I want to switch between light and dark themes.

#### Acceptance Criteria

1. WHEN an admin clicks theme toggle THEN the system SHALL switch Bootstrap themes
2. WHEN theme is changed THEN the system SHALL persist preference in localStorage
3. WHEN admin revisits THEN the system SHALL apply previously selected theme

---

### Requirement 7: Public Frontend Music Player

**User Story:** As a visitor, I want to access playlist URLs and play music.

#### Acceptance Criteria

1. WHEN a visitor accesses /playlist/{slug} THEN the system SHALL display HTML5 audio player
2. WHEN playlist page loads THEN the system SHALL display playlist name and song list
3. WHEN a song finishes THEN the player SHALL auto-advance to next song
4. WHEN a visitor clicks a song THEN the player SHALL start playing that song
5. The frontend SHALL NOT require authentication

---

### Requirement 8: Modern Bootstrap UI

**User Story:** As a user, I want a clean, modern Bootstrap interface.

#### Acceptance Criteria

1. The backend interface SHALL use Bootstrap 5 with modern design
2. The frontend player SHALL use Bootstrap 5 with responsive design
3. The interface SHALL work on desktop and mobile devices
4. The UI SHALL include navigation, forms, and visual feedback
