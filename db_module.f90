! db_module.f90 - SQLite database operations module
! Handles all database CRUD operations for playlists and songs

module db_module
    use, intrinsic :: iso_c_binding
    implicit none
    private
    
    ! SQLite C interface bindings
    integer(c_int), parameter :: SQLITE_OK = 0
    integer(c_int), parameter :: SQLITE_ROW = 100
    integer(c_int), parameter :: SQLITE_DONE = 101
    
    interface
        function sqlite3_open(filename, ppDb) bind(C, name='sqlite3_open')
            import :: c_ptr, c_char, c_int
            character(kind=c_char), intent(in) :: filename(*)
            type(c_ptr), intent(out) :: ppDb
            integer(c_int) :: sqlite3_open
        end function
        
        function sqlite3_close(db) bind(C, name='sqlite3_close')
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int) :: sqlite3_close
        end function
        
        function sqlite3_exec(db, sql, callback, arg, errmsg) bind(C, name='sqlite3_exec')
            import :: c_ptr, c_char, c_int, c_funptr
            type(c_ptr), value :: db
            character(kind=c_char), intent(in) :: sql(*)
            type(c_funptr), value :: callback
            type(c_ptr), value :: arg
            type(c_ptr), intent(out) :: errmsg
            integer(c_int) :: sqlite3_exec
        end function
        
        function sqlite3_prepare_v2(db, sql, nByte, ppStmt, pzTail) bind(C, name='sqlite3_prepare_v2')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: db
            character(kind=c_char), intent(in) :: sql(*)
            integer(c_int), value :: nByte
            type(c_ptr), intent(out) :: ppStmt
            type(c_ptr), intent(out) :: pzTail
            integer(c_int) :: sqlite3_prepare_v2
        end function
        
        function sqlite3_step(stmt) bind(C, name='sqlite3_step')
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int) :: sqlite3_step
        end function
        
        function sqlite3_finalize(stmt) bind(C, name='sqlite3_finalize')
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int) :: sqlite3_finalize
        end function
        
        function sqlite3_column_int(stmt, iCol) bind(C, name='sqlite3_column_int')
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int), value :: iCol
            integer(c_int) :: sqlite3_column_int
        end function
        
        function sqlite3_column_text(stmt, iCol) bind(C, name='sqlite3_column_text')
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int), value :: iCol
            type(c_ptr) :: sqlite3_column_text
        end function
        
        function sqlite3_bind_int(stmt, idx, val) bind(C, name='sqlite3_bind_int')
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int), value :: idx, val
            integer(c_int) :: sqlite3_bind_int
        end function
        
        function sqlite3_bind_text(stmt, idx, text, nByte, destructor) bind(C, name='sqlite3_bind_text')
            import :: c_ptr, c_char, c_int, c_funptr
            type(c_ptr), value :: stmt
            integer(c_int), value :: idx, nByte
            character(kind=c_char), intent(in) :: text(*)
            type(c_funptr), value :: destructor
            integer(c_int) :: sqlite3_bind_text
        end function
        
        function sqlite3_last_insert_rowid(db) bind(C, name='sqlite3_last_insert_rowid')
            import :: c_ptr, c_int64_t
            type(c_ptr), value :: db
            integer(c_int64_t) :: sqlite3_last_insert_rowid
        end function
        
        function sqlite3_reset(stmt) bind(C, name='sqlite3_reset')
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int) :: sqlite3_reset
        end function
    end interface
    
    ! Database handle
    type(c_ptr), save :: db = c_null_ptr
    character(len=*), parameter :: DB_FILE = 'music.db'
    
    ! Playlist type
    type, public :: playlist_t
        integer :: id = 0
        character(len=256) :: name = ''
        character(len=1024) :: description = ''
        character(len=32) :: created_at = ''
        character(len=32) :: updated_at = ''
    end type
    
    ! Song type
    type, public :: song_t
        integer :: id = 0
        character(len=256) :: filename = ''
        character(len=256) :: title = ''
        character(len=512) :: filepath = ''
        character(len=32) :: created_at = ''
        integer :: position = 0
    end type
    
    public :: db_init, db_close
    public :: get_all_playlists, get_playlist, create_playlist, update_playlist, delete_playlist
    public :: get_playlist_songs, add_song_to_playlist, remove_song_from_playlist
    public :: create_song, get_song, delete_song, update_song_order
    public :: get_playlist_count, get_song_count, get_song_count_in_playlist
    
contains

    ! Helper: Convert C string pointer to Fortran string
    subroutine c_str_to_fortran(ptr, fstr)
        type(c_ptr), intent(in) :: ptr
        character(len=*), intent(out) :: fstr
        character(kind=c_char), pointer :: cstr(:)
        integer :: i, slen, maxlen
        
        fstr = ''
        if (.not. c_associated(ptr)) return
        
        maxlen = len(fstr)
        call c_f_pointer(ptr, cstr, [maxlen])
        
        ! Find null terminator and copy characters
        slen = 0
        do i = 1, maxlen
            if (cstr(i) == c_null_char) exit
            slen = i
        end do
        
        do i = 1, slen
            fstr(i:i) = cstr(i)
        end do
    end subroutine c_str_to_fortran
    
    ! Initialize database and create tables
    subroutine db_init()
        integer(c_int) :: rc
        type(c_ptr) :: errmsg
        character(len=2048) :: sql
        
        ! Open database
        rc = sqlite3_open(DB_FILE // c_null_char, db)
        if (rc /= SQLITE_OK) then
            print *, 'Error: Cannot open database'
            return
        end if
        
        ! Create tables
        sql = 'CREATE TABLE IF NOT EXISTS playlists (' // &
              'id INTEGER PRIMARY KEY AUTOINCREMENT,' // &
              'name TEXT NOT NULL,' // &
              'description TEXT,' // &
              'created_at TEXT DEFAULT CURRENT_TIMESTAMP,' // &
              'updated_at TEXT DEFAULT CURRENT_TIMESTAMP);' // &
              'CREATE TABLE IF NOT EXISTS songs (' // &
              'id INTEGER PRIMARY KEY AUTOINCREMENT,' // &
              'filename TEXT NOT NULL,' // &
              'title TEXT,' // &
              'filepath TEXT NOT NULL,' // &
              'created_at TEXT DEFAULT CURRENT_TIMESTAMP);' // &
              'CREATE TABLE IF NOT EXISTS playlist_songs (' // &
              'playlist_id INTEGER,' // &
              'song_id INTEGER,' // &
              'position INTEGER DEFAULT 0,' // &
              'PRIMARY KEY (playlist_id, song_id),' // &
              'FOREIGN KEY (playlist_id) REFERENCES playlists(id) ON DELETE CASCADE,' // &
              'FOREIGN KEY (song_id) REFERENCES songs(id) ON DELETE CASCADE);'
        
        rc = sqlite3_exec(db, trim(sql) // c_null_char, c_null_funptr, c_null_ptr, errmsg)
        if (rc /= SQLITE_OK) then
            print *, 'Warning: Table creation returned non-OK status'
        end if
    end subroutine db_init
    
    ! Close database connection
    subroutine db_close()
        integer(c_int) :: rc
        if (c_associated(db)) then
            rc = sqlite3_close(db)
        end if
    end subroutine db_close
    
    ! Get count of playlists
    function get_playlist_count() result(count)
        integer :: count
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=64) :: sql
        
        count = 0
        sql = 'SELECT COUNT(*) FROM playlists' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc == SQLITE_OK) then
            if (sqlite3_step(stmt) == SQLITE_ROW) then
                count = sqlite3_column_int(stmt, 0_c_int)
            end if
            rc = sqlite3_finalize(stmt)
        end if
    end function get_playlist_count
    
    ! Get count of songs
    function get_song_count() result(count)
        integer :: count
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=64) :: sql
        
        count = 0
        sql = 'SELECT COUNT(*) FROM songs' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc == SQLITE_OK) then
            if (sqlite3_step(stmt) == SQLITE_ROW) then
                count = sqlite3_column_int(stmt, 0_c_int)
            end if
            rc = sqlite3_finalize(stmt)
        end if
    end function get_song_count
    
    ! Get count of songs in a specific playlist
    function get_song_count_in_playlist(playlist_id) result(count)
        integer, intent(in) :: playlist_id
        integer :: count
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=128) :: sql
        
        count = 0
        sql = 'SELECT COUNT(*) FROM playlist_songs WHERE playlist_id = ?' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc == SQLITE_OK) then
            rc = sqlite3_bind_int(stmt, 1_c_int, int(playlist_id, c_int))
            if (sqlite3_step(stmt) == SQLITE_ROW) then
                count = sqlite3_column_int(stmt, 0_c_int)
            end if
            rc = sqlite3_finalize(stmt)
        end if
    end function get_song_count_in_playlist
    
    ! Get all playlists
    subroutine get_all_playlists(playlists, count)
        type(playlist_t), intent(out) :: playlists(:)
        integer, intent(out) :: count
        type(c_ptr) :: stmt, tail, text_ptr
        integer(c_int) :: rc
        character(len=256) :: sql
        integer :: i
        
        count = 0
        sql = 'SELECT id, name, description, created_at, updated_at FROM playlists ORDER BY id DESC' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        i = 1
        do while (sqlite3_step(stmt) == SQLITE_ROW .and. i <= size(playlists))
            playlists(i)%id = sqlite3_column_int(stmt, 0_c_int)
            
            text_ptr = sqlite3_column_text(stmt, 1_c_int)
            call c_str_to_fortran(text_ptr, playlists(i)%name)
            
            text_ptr = sqlite3_column_text(stmt, 2_c_int)
            call c_str_to_fortran(text_ptr, playlists(i)%description)
            
            i = i + 1
        end do
        
        count = i - 1
        rc = sqlite3_finalize(stmt)
    end subroutine get_all_playlists
    
    ! Get single playlist by ID
    subroutine get_playlist(id, playlist, found)
        integer, intent(in) :: id
        type(playlist_t), intent(out) :: playlist
        logical, intent(out) :: found
        type(c_ptr) :: stmt, tail, text_ptr
        integer(c_int) :: rc
        character(len=256) :: sql
        
        found = .false.
        sql = 'SELECT id, name, description, created_at, updated_at FROM playlists WHERE id = ?' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_int(stmt, 1_c_int, int(id, c_int))
        
        if (sqlite3_step(stmt) == SQLITE_ROW) then
            found = .true.
            playlist%id = sqlite3_column_int(stmt, 0_c_int)
            
            text_ptr = sqlite3_column_text(stmt, 1_c_int)
            call c_str_to_fortran(text_ptr, playlist%name)
            
            text_ptr = sqlite3_column_text(stmt, 2_c_int)
            call c_str_to_fortran(text_ptr, playlist%description)
        end if
        
        rc = sqlite3_finalize(stmt)
    end subroutine get_playlist
    
    ! Create new playlist
    function create_playlist(name, description) result(id)
        character(len=*), intent(in) :: name, description
        integer :: id
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=512) :: sql
        character(len=1024), target, save :: name_buf, desc_buf
        
        id = 0
        sql = 'INSERT INTO playlists (name, description) VALUES (?, ?)' // c_null_char
        
        ! Copy to persistent buffers with null terminator
        name_buf = trim(name) // c_null_char
        desc_buf = trim(description) // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_text(stmt, 1_c_int, name_buf, int(len_trim(name), c_int), c_null_funptr)
        rc = sqlite3_bind_text(stmt, 2_c_int, desc_buf, int(len_trim(description), c_int), c_null_funptr)
        
        if (sqlite3_step(stmt) == SQLITE_DONE) then
            id = int(sqlite3_last_insert_rowid(db))
        end if
        
        rc = sqlite3_finalize(stmt)
    end function create_playlist
    
    ! Update playlist
    subroutine update_playlist(id, name, description, success)
        integer, intent(in) :: id
        character(len=*), intent(in) :: name, description
        logical, intent(out) :: success
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=512) :: sql
        character(len=1024), target, save :: name_buf, desc_buf
        
        success = .false.
        sql = 'UPDATE playlists SET name = ?, description = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?' // c_null_char
        
        ! Copy to persistent buffers
        name_buf = trim(name) // c_null_char
        desc_buf = trim(description) // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_text(stmt, 1_c_int, name_buf, int(len_trim(name), c_int), c_null_funptr)
        rc = sqlite3_bind_text(stmt, 2_c_int, desc_buf, int(len_trim(description), c_int), c_null_funptr)
        rc = sqlite3_bind_int(stmt, 3_c_int, int(id, c_int))
        
        if (sqlite3_step(stmt) == SQLITE_DONE) then
            success = .true.
        end if
        
        rc = sqlite3_finalize(stmt)
    end subroutine update_playlist
    
    ! Delete playlist
    subroutine delete_playlist(id, success)
        integer, intent(in) :: id
        logical, intent(out) :: success
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=256) :: sql
        
        success = .false.
        
        ! First delete playlist_songs entries
        sql = 'DELETE FROM playlist_songs WHERE playlist_id = ?' // c_null_char
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc == SQLITE_OK) then
            rc = sqlite3_bind_int(stmt, 1_c_int, int(id, c_int))
            rc = sqlite3_step(stmt)
            rc = sqlite3_finalize(stmt)
        end if
        
        ! Then delete playlist
        sql = 'DELETE FROM playlists WHERE id = ?' // c_null_char
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_int(stmt, 1_c_int, int(id, c_int))
        
        if (sqlite3_step(stmt) == SQLITE_DONE) then
            success = .true.
        end if
        
        rc = sqlite3_finalize(stmt)
    end subroutine delete_playlist
    
    ! Get songs in a playlist
    subroutine get_playlist_songs(playlist_id, songs, count)
        integer, intent(in) :: playlist_id
        type(song_t), intent(out) :: songs(:)
        integer, intent(out) :: count
        type(c_ptr) :: stmt, tail, text_ptr
        integer(c_int) :: rc
        character(len=512) :: sql
        integer :: i
        
        count = 0
        sql = 'SELECT s.id, s.filename, s.title, s.filepath, ps.position ' // &
              'FROM songs s JOIN playlist_songs ps ON s.id = ps.song_id ' // &
              'WHERE ps.playlist_id = ? ORDER BY ps.position' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_int(stmt, 1_c_int, int(playlist_id, c_int))
        
        i = 1
        do while (sqlite3_step(stmt) == SQLITE_ROW .and. i <= size(songs))
            songs(i)%id = sqlite3_column_int(stmt, 0_c_int)
            
            text_ptr = sqlite3_column_text(stmt, 1_c_int)
            call c_str_to_fortran(text_ptr, songs(i)%filename)
            
            text_ptr = sqlite3_column_text(stmt, 2_c_int)
            call c_str_to_fortran(text_ptr, songs(i)%title)
            
            text_ptr = sqlite3_column_text(stmt, 3_c_int)
            call c_str_to_fortran(text_ptr, songs(i)%filepath)
            
            songs(i)%position = sqlite3_column_int(stmt, 4_c_int)
            
            i = i + 1
        end do
        
        count = i - 1
        rc = sqlite3_finalize(stmt)
    end subroutine get_playlist_songs
    
    ! Create new song
    function create_song(filename, title, filepath) result(id)
        character(len=*), intent(in) :: filename, title, filepath
        integer :: id
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=512) :: sql
        character(len=1024), target, save :: fn_buf, title_buf, path_buf
        
        id = 0
        sql = 'INSERT INTO songs (filename, title, filepath) VALUES (?, ?, ?)' // c_null_char
        
        ! Copy to persistent buffers
        fn_buf = trim(filename) // c_null_char
        title_buf = trim(title) // c_null_char
        path_buf = trim(filepath) // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_text(stmt, 1_c_int, fn_buf, int(len_trim(filename), c_int), c_null_funptr)
        rc = sqlite3_bind_text(stmt, 2_c_int, title_buf, int(len_trim(title), c_int), c_null_funptr)
        rc = sqlite3_bind_text(stmt, 3_c_int, path_buf, int(len_trim(filepath), c_int), c_null_funptr)
        
        if (sqlite3_step(stmt) == SQLITE_DONE) then
            id = int(sqlite3_last_insert_rowid(db))
        end if
        
        rc = sqlite3_finalize(stmt)
    end function create_song
    
    ! Get single song by ID
    subroutine get_song(id, song, found)
        integer, intent(in) :: id
        type(song_t), intent(out) :: song
        logical, intent(out) :: found
        type(c_ptr) :: stmt, tail, text_ptr
        integer(c_int) :: rc
        character(len=256) :: sql
        
        found = .false.
        sql = 'SELECT id, filename, title, filepath FROM songs WHERE id = ?' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_int(stmt, 1_c_int, int(id, c_int))
        
        if (sqlite3_step(stmt) == SQLITE_ROW) then
            found = .true.
            song%id = sqlite3_column_int(stmt, 0_c_int)
            
            text_ptr = sqlite3_column_text(stmt, 1_c_int)
            call c_str_to_fortran(text_ptr, song%filename)
            
            text_ptr = sqlite3_column_text(stmt, 2_c_int)
            call c_str_to_fortran(text_ptr, song%title)
            
            text_ptr = sqlite3_column_text(stmt, 3_c_int)
            call c_str_to_fortran(text_ptr, song%filepath)
        end if
        
        rc = sqlite3_finalize(stmt)
    end subroutine get_song
    
    ! Delete song
    subroutine delete_song(id, success)
        integer, intent(in) :: id
        logical, intent(out) :: success
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=256) :: sql
        
        success = .false.
        
        ! First delete from playlist_songs
        sql = 'DELETE FROM playlist_songs WHERE song_id = ?' // c_null_char
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc == SQLITE_OK) then
            rc = sqlite3_bind_int(stmt, 1_c_int, int(id, c_int))
            rc = sqlite3_step(stmt)
            rc = sqlite3_finalize(stmt)
        end if
        
        ! Then delete song
        sql = 'DELETE FROM songs WHERE id = ?' // c_null_char
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_int(stmt, 1_c_int, int(id, c_int))
        
        if (sqlite3_step(stmt) == SQLITE_DONE) then
            success = .true.
        end if
        
        rc = sqlite3_finalize(stmt)
    end subroutine delete_song
    
    ! Add song to playlist
    subroutine add_song_to_playlist(playlist_id, song_id, position, success)
        integer, intent(in) :: playlist_id, song_id, position
        logical, intent(out) :: success
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=256) :: sql
        
        success = .false.
        sql = 'INSERT OR REPLACE INTO playlist_songs (playlist_id, song_id, position) VALUES (?, ?, ?)' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_int(stmt, 1_c_int, int(playlist_id, c_int))
        rc = sqlite3_bind_int(stmt, 2_c_int, int(song_id, c_int))
        rc = sqlite3_bind_int(stmt, 3_c_int, int(position, c_int))
        
        if (sqlite3_step(stmt) == SQLITE_DONE) then
            success = .true.
        end if
        
        rc = sqlite3_finalize(stmt)
    end subroutine add_song_to_playlist
    
    ! Remove song from playlist
    subroutine remove_song_from_playlist(playlist_id, song_id, success)
        integer, intent(in) :: playlist_id, song_id
        logical, intent(out) :: success
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=256) :: sql
        
        success = .false.
        sql = 'DELETE FROM playlist_songs WHERE playlist_id = ? AND song_id = ?' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) return
        
        rc = sqlite3_bind_int(stmt, 1_c_int, int(playlist_id, c_int))
        rc = sqlite3_bind_int(stmt, 2_c_int, int(song_id, c_int))
        
        if (sqlite3_step(stmt) == SQLITE_DONE) then
            success = .true.
        end if
        
        rc = sqlite3_finalize(stmt)
    end subroutine remove_song_from_playlist
    
    ! Update song order in playlist
    subroutine update_song_order(playlist_id, song_ids, count, success)
        integer, intent(in) :: playlist_id, count
        integer, intent(in) :: song_ids(:)
        logical, intent(out) :: success
        type(c_ptr) :: stmt, tail
        integer(c_int) :: rc
        character(len=256) :: sql
        integer :: i
        
        success = .true.
        sql = 'UPDATE playlist_songs SET position = ? WHERE playlist_id = ? AND song_id = ?' // c_null_char
        
        rc = sqlite3_prepare_v2(db, sql, -1_c_int, stmt, tail)
        if (rc /= SQLITE_OK) then
            success = .false.
            return
        end if
        
        do i = 1, count
            rc = sqlite3_bind_int(stmt, 1_c_int, int(i, c_int))
            rc = sqlite3_bind_int(stmt, 2_c_int, int(playlist_id, c_int))
            rc = sqlite3_bind_int(stmt, 3_c_int, int(song_ids(i), c_int))
            
            if (sqlite3_step(stmt) /= SQLITE_DONE) then
                success = .false.
            end if
            
            rc = sqlite3_reset(stmt)
        end do
        
        rc = sqlite3_finalize(stmt)
    end subroutine update_song_order

end module db_module
