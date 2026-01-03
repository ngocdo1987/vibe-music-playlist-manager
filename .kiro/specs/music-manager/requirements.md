# Requirements Document

## Introduction

This document outlines the requirements for a Lisp-based web application that manages MP3 song files and playlists. The application consists of a public frontend for music playback and a protected backend for administration. It uses SQLite for data storage and Bootstrap for the UI with light/dark theme support.

## Requirements

### Requirement 1: Database and Data Model

**User Story:** As a developer, I want a SQLite database with proper schema, so that playlists and songs can be stored with many-to-many relationships.

#### Acceptance Criteria

1. WHEN the application starts THEN the system SHALL create SQLite database with tables for playlists, songs, and playlist_songs junction table
2. WHEN a playlist is created THEN the system SHALL store playlist id, name, and creation timestamp
3. WHEN a song is uploaded THEN the system SHALL store song id, filename, original name, and upload timestamp
4. WHEN songs are added to a playlist THEN the system SHALL store the relationship with display order

---

### Requirement 2: Backend Authentication

**User Story:** As an admin, I want to log in with username and password, so that only authorized users can manage playlists.

#### Acceptance Criteria

1. WHEN accessing any backend route THEN the system SHALL redirect unauthenticated users to the login page
2. WHEN submitting valid credentials from .env file THEN the system SHALL create a session and redirect to admin dashboard
3. WHEN submitting invalid credentials THEN the system SHALL display an error message and remain on login page
4. WHEN clicking logout THEN the system SHALL destroy the session and redirect to login page
5. IF .env file is missing credentials THEN the system SHALL refuse to start and display configuration error

---

### Requirement 3: Playlist Management

**User Story:** As an admin, I want to create, edit, and delete playlists, so that I can organize my music collection.

#### Acceptance Criteria

1. WHEN viewing admin dashboard THEN the system SHALL display a list of all playlists with edit and delete actions
2. WHEN clicking create playlist THEN the system SHALL display a form with playlist name field and song upload area
3. WHEN submitting a new playlist THEN the system SHALL save the playlist to database and redirect to dashboard
4. WHEN clicking edit on a playlist THEN the system SHALL display the edit form with current name and songs
5. WHEN submitting edited playlist THEN the system SHALL update the playlist in database
6. WHEN clicking delete on a playlist THEN the system SHALL prompt for confirmation before removing
7. WHEN confirming delete THEN the system SHALL remove the playlist and its song associations from database

---

### Requirement 4: MP3 File Upload and Validation

**User Story:** As an admin, I want to upload MP3 files with validation, so that only valid audio files are stored.

#### Acceptance Criteria

1. WHEN uploading files THEN the system SHALL accept multiple file selection
2. WHEN a file is uploaded THEN the system SHALL validate the file has .mp3 extension
3. WHEN a file is uploaded THEN the system SHALL validate the file contains valid MP3 magic bytes (ID3 or 0xFF 0xFB)
4. IF file validation fails THEN the system SHALL display an error message and reject the file
5. WHEN validation passes THEN the system SHALL save the file to the "mp3" folder on the server
6. WHEN saving a file THEN the system SHALL generate a unique filename to prevent conflicts

---

### Requirement 5: Song Ordering with Drag and Drop

**User Story:** As an admin, I want to drag and drop songs to reorder them, so that I can arrange the playlist sequence.

#### Acceptance Criteria

1. WHEN viewing playlist edit form THEN the system SHALL display songs in a sortable list
2. WHEN dragging a song item THEN the system SHALL provide visual feedback of the drag operation
3. WHEN dropping a song at new position THEN the system SHALL update the display order
4. WHEN saving playlist THEN the system SHALL persist the new song order to database

---

### Requirement 6: Backend Theme Switching

**User Story:** As an admin, I want to switch between light and dark themes, so that I can use my preferred visual style.

#### Acceptance Criteria

1. WHEN viewing any backend page THEN the system SHALL display a theme toggle button
2. WHEN clicking theme toggle THEN the system SHALL switch between Bootstrap light and dark themes
3. WHEN switching theme THEN the system SHALL persist the preference in browser localStorage
4. WHEN loading a page THEN the system SHALL apply the saved theme preference

---

### Requirement 7: Public Frontend Playlist Playback

**User Story:** As a visitor, I want to access playlist URLs and play music, so that I can listen to the playlists.

#### Acceptance Criteria

1. WHEN accessing a playlist URL THEN the system SHALL display the playlist without requiring login
2. WHEN viewing a playlist THEN the system SHALL display an HTML5 audio player with playlist name
3. WHEN viewing a playlist THEN the system SHALL display the list of songs in stored order
4. WHEN a song ends THEN the player SHALL automatically play the next song in the playlist
5. WHEN clicking a song in the list THEN the player SHALL play that song
6. IF playlist URL is invalid THEN the system SHALL display a 404 error page

---

### Requirement 8: Modern Bootstrap UI

**User Story:** As a user, I want a clean and modern interface, so that the application is pleasant to use.

#### Acceptance Criteria

1. WHEN rendering any page THEN the system SHALL use Bootstrap 5 for styling
2. WHEN viewing on mobile THEN the system SHALL display responsive layouts
3. WHEN viewing backend THEN the system SHALL display a clean admin dashboard with navigation
4. WHEN viewing frontend THEN the system SHALL display an attractive music player interface

---

### Requirement 9: Code and Documentation Standards

**User Story:** As a developer, I want English code and comments, so that the codebase is maintainable.

#### Acceptance Criteria

1. WHEN writing code THEN all comments SHALL be in English
2. WHEN displaying messages THEN all user-facing text SHALL be in English
3. WHEN organizing files THEN the system SHALL follow a logical project structure
4. WHEN deploying THEN step-by-step Linux server instructions SHALL be provided
