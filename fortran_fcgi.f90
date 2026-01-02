! fortran_fcgi.f90 - Main FastCGI controller with template support
! Handles HTTP routing, request processing, and response generation
! Uses separate HTML template files for easier maintenance

program fortran_fcgi
    use, intrinsic :: iso_c_binding
    use env_module
    use db_module
    use auth_module
    use mp3_module
    use template_module
    implicit none
    
    ! FastCGI C interface bindings
    interface
        function FCGX_Init() bind(C, name='FCGX_Init')
            import :: c_int
            integer(c_int) :: FCGX_Init
        end function
        
        function FCGX_Accept(stdin, stdout, stderr, envp) bind(C, name='FCGX_Accept')
            import :: c_int, c_ptr
            type(c_ptr), intent(out) :: stdin, stdout, stderr, envp
            integer(c_int) :: FCGX_Accept
        end function
        
        function FCGX_GetParam(name, envp) bind(C, name='FCGX_GetParam')
            import :: c_char, c_ptr
            character(kind=c_char), intent(in) :: name(*)
            type(c_ptr), value :: envp
            type(c_ptr) :: FCGX_GetParam
        end function
        
        function FCGX_PutS(str, stream) bind(C, name='FCGX_PutS')
            import :: c_char, c_ptr, c_int
            character(kind=c_char), intent(in) :: str(*)
            type(c_ptr), value :: stream
            integer(c_int) :: FCGX_PutS
        end function
        
        function FCGX_GetLine(str, n, stream) bind(C, name='FCGX_GetLine')
            import :: c_char, c_ptr, c_int
            character(kind=c_char), intent(out) :: str(*)
            integer(c_int), value :: n
            type(c_ptr), value :: stream
            type(c_ptr) :: FCGX_GetLine
        end function
        
        function FCGX_GetChar(stream) bind(C, name='FCGX_GetChar')
            import :: c_ptr, c_int
            type(c_ptr), value :: stream
            integer(c_int) :: FCGX_GetChar
        end function
    end interface
    
    type(c_ptr) :: stdin_ptr, stdout_ptr, stderr_ptr, envp
    character(len=2048) :: request_uri, request_method, query_string
    character(len=2048) :: content_type, cookie_header, content_length_str
    character(len=8192) :: post_data
    character(len=64) :: session_token
    integer(c_int) :: rc
    integer :: content_length
    logical :: is_authenticated
    
    ! Initialize
    rc = FCGX_Init()
    call load_env()
    call db_init()
    call auth_init()
    
    ! Main request loop
    do while (FCGX_Accept(stdin_ptr, stdout_ptr, stderr_ptr, envp) >= 0)
        ! Get request parameters
        request_uri = get_env_var('REQUEST_URI')
        request_method = get_env_var('REQUEST_METHOD')
        query_string = get_env_var('QUERY_STRING')
        content_type = get_env_var('CONTENT_TYPE')
        cookie_header = get_env_var('HTTP_COOKIE')
        content_length_str = get_env_var('CONTENT_LENGTH')
        
        ! Parse content length
        content_length = 0
        if (len_trim(content_length_str) > 0) then
            read(content_length_str, *, err=10) content_length
        end if
        10 continue
        
        ! Read POST data if NOT multipart (multipart is read by upload handler)
        post_data = ''
        if (content_length > 0 .and. content_length < len(post_data)) then
            if (index(content_type, 'multipart/form-data') == 0) then
                call read_post_data(stdin_ptr, post_data, content_length)
            end if
        end if
        
        ! Extract session token from cookies
        session_token = extract_cookie(cookie_header, 'session_token')
        is_authenticated = check_session(session_token)
        
        ! Route the request
        call route_request(request_uri, request_method, post_data, &
                          is_authenticated, session_token, stdout_ptr, &
                          stdin_ptr, content_type, content_length)
    end do
    
    call db_close()

contains

    ! Get environment variable as Fortran string
    function get_env_var(name) result(value)
        character(len=*), intent(in) :: name
        character(len=2048) :: value
        type(c_ptr) :: ptr
        character(kind=c_char), pointer :: cstr(:)
        integer :: i, slen
        
        value = ''
        ptr = FCGX_GetParam(trim(name) // c_null_char, envp)
        if (c_associated(ptr)) then
            ! Convert C string to Fortran - find null terminator
            call c_f_pointer(ptr, cstr, [2048])
            slen = 0
            do i = 1, 2048
                if (cstr(i) == c_null_char) exit
                slen = i
            end do
            ! Copy characters
            do i = 1, slen
                value(i:i) = cstr(i)
            end do
        end if
    end function get_env_var
    
    ! Read POST data from stdin
    subroutine read_post_data(stream, data, length)
        type(c_ptr), intent(in) :: stream
        character(len=*), intent(out) :: data
        integer, intent(in) :: length
        character(len=1) :: ch
        integer :: i, max_len
        
        data = ''
        
        ! Handle edge cases
        if (length <= 0) return
        max_len = min(length, len(data))
        if (max_len <= 0) return
        
        do i = 1, max_len
            if (.not. c_associated(FCGX_GetLine(ch, 2_c_int, stream))) exit
            data(i:i) = ch
        end do
    end subroutine read_post_data
    
    ! Extract cookie value by name
    function extract_cookie(cookies, name) result(value)
        character(len=*), intent(in) :: cookies, name
        character(len=64) :: value
        integer :: start_pos, end_pos
        character(len=256) :: search_str
        
        value = ''
        search_str = trim(name) // '='
        start_pos = index(cookies, trim(search_str))
        
        if (start_pos > 0) then
            start_pos = start_pos + len_trim(search_str)
            end_pos = index(cookies(start_pos:), ';')
            if (end_pos == 0) then
                value = trim(cookies(start_pos:))
            else
                value = cookies(start_pos:start_pos + end_pos - 2)
            end if
        end if
    end function extract_cookie
    
    ! Main routing logic
    subroutine route_request(uri, method, post_data, authenticated, token, stream, &
                            in_stream, in_content_type, in_content_length)
        character(len=*), intent(in) :: uri, method, post_data
        character(len=*), intent(in) :: token, in_content_type
        logical, intent(in) :: authenticated
        type(c_ptr), intent(in) :: stream, in_stream
        integer, intent(in) :: in_content_length
        character(len=256) :: path
        integer :: id, qmark_pos, dummy_len
        
        ! Suppress unused variable warning
        dummy_len = len_trim(token)
        
        ! Remove query string from URI
        qmark_pos = index(uri, '?')
        if (qmark_pos > 0) then
            path = trim(adjustl(uri(1:qmark_pos-1)))
        else
            path = trim(adjustl(uri))
        end if
        
        ! Public routes
        if (trim(path) == '/' .or. len_trim(path) == 0) then
            call handle_public_index(stream)
            return
        end if
        
        if (len_trim(path) >= 10) then
            if (path(1:10) == '/playlist/') then
                id = parse_id_from_path(path, 10)
                if (id > 0) then
                    call handle_public_player(id, stream)
                    return
                end if
            end if
        end if
        
        ! Admin login/logout
        if (path == '/admin/login') then
            if (method == 'POST') then
                call handle_login_post(post_data, stream)
            else
                call handle_login_form(stream)
            end if
            return
        end if
        
        if (path == '/admin/logout') then
            call handle_logout(stream)
            return
        end if
        
        ! Protected admin routes - check authentication
        if (path(1:6) == '/admin') then
            if (.not. authenticated) then
                call redirect('/admin/login', stream)
                return
            end if
            
            ! Admin dashboard
            if (path == '/admin' .or. path == '/admin/') then
                call handle_admin_dashboard(stream)
                return
            end if
            
            ! Playlist CRUD
            if (path == '/admin/playlist/new') then
                if (method == 'POST') then
                    call handle_playlist_create(post_data, stream)
                else
                    call handle_playlist_form(0, stream)
                end if
                return
            end if
            
            if (index(path, '/admin/playlist/') == 1 .and. index(path, '/edit') > 0) then
                id = parse_id_from_path(path, 16)
                if (id > 0) then
                    if (method == 'POST') then
                        call handle_playlist_update(id, post_data, stream)
                    else
                        call handle_playlist_form(id, stream)
                    end if
                    return
                end if
            end if
            
            if (index(path, '/admin/playlist/') == 1 .and. index(path, '/delete') > 0) then
                id = parse_id_from_path(path, 16)
                if (id > 0) then
                    call handle_playlist_delete(id, stream)
                    return
                end if
            end if
            
            if (index(path, '/admin/playlist/') == 1 .and. index(path, '/reorder') > 0) then
                id = parse_id_from_path(path, 16)
                if (id > 0 .and. method == 'POST') then
                    call handle_song_reorder(id, post_data, stream)
                    return
                end if
            end if
            
            if (path == '/admin/upload' .and. method == 'POST') then
                call handle_file_upload(in_stream, in_content_type, in_content_length, stream)
                return
            end if
            
            if (index(path, '/admin/song/') == 1 .and. index(path, '/remove/') > 0) then
                call handle_remove_song_from_playlist(path, stream)
                return
            end if
        end if
        
        ! 404 Not Found
        call handle_404(stream)
    end subroutine route_request
    
    ! Parse ID from URL path
    function parse_id_from_path(path, start) result(id)
        character(len=*), intent(in) :: path
        integer, intent(in) :: start
        integer :: id, end_pos
        character(len=16) :: id_str
        
        id = 0
        end_pos = index(path(start+1:), '/')
        if (end_pos == 0) then
            id_str = path(start+1:)
        else
            id_str = path(start+1:start+end_pos-1)
        end if
        
        read(id_str, *, err=20) id
        20 continue
    end function parse_id_from_path
    
    ! Send HTTP redirect
    subroutine redirect(location, stream)
        character(len=*), intent(in) :: location
        type(c_ptr), intent(in) :: stream
        integer(c_int) :: rc
        
        rc = FCGX_PutS('Status: 302 Found' // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS('Location: ' // trim(location) // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS('Content-Type: text/html' // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS(char(13) // char(10) // c_null_char, stream)
    end subroutine redirect
    
    ! Send HTTP redirect with cookie
    subroutine redirect_with_cookie(location, cookie, stream)
        character(len=*), intent(in) :: location, cookie
        type(c_ptr), intent(in) :: stream
        integer(c_int) :: rc
        
        rc = FCGX_PutS('Status: 302 Found' // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS('Location: ' // trim(location) // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS('Set-Cookie: ' // trim(cookie) // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS('Content-Type: text/html' // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS(char(13) // char(10) // c_null_char, stream)
    end subroutine redirect_with_cookie
    
    ! Output HTML header
    subroutine html_header(stream)
        type(c_ptr), intent(in) :: stream
        integer(c_int) :: rc
        
        rc = FCGX_PutS('Content-Type: text/html; charset=utf-8' // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS(char(13) // char(10) // c_null_char, stream)
    end subroutine html_header
    
    ! Output JSON header
    subroutine json_header(stream)
        type(c_ptr), intent(in) :: stream
        integer(c_int) :: rc
        
        rc = FCGX_PutS('Content-Type: application/json' // char(13) // char(10) // c_null_char, stream)
        rc = FCGX_PutS(char(13) // char(10) // c_null_char, stream)
    end subroutine json_header
    
    ! Output string to response
    subroutine output(str, stream)
        character(len=*), intent(in) :: str
        type(c_ptr), intent(in) :: stream
        integer(c_int) :: rc
        
        rc = FCGX_PutS(trim(str) // c_null_char, stream)
    end subroutine output
    
    ! Parse form field value
    function parse_form_value(data, field) result(value)
        character(len=*), intent(in) :: data, field
        character(len=1024) :: value
        integer :: start_pos, end_pos
        character(len=256) :: search_str
        
        value = ''
        search_str = trim(field) // '='
        start_pos = index(data, trim(search_str))
        
        if (start_pos > 0) then
            start_pos = start_pos + len_trim(search_str)
            end_pos = index(data(start_pos:), '&')
            if (end_pos == 0) then
                value = data(start_pos:)
            else
                value = data(start_pos:start_pos + end_pos - 2)
            end if
            ! URL decode (basic - replace + with space)
            do while (index(value, '+') > 0)
                value(index(value, '+'):index(value, '+')) = ' '
            end do
        end if
    end function parse_form_value
    
    ! Handle public index page
    subroutine handle_public_index(stream)
        type(c_ptr), intent(in) :: stream
        type(playlist_t), save :: playlists(100)
        integer :: count, i
        character(len=16) :: id_str
        character(len=65536) :: html_output, cards_html
        character(len=4096) :: card_html
        logical :: success
        
        call html_header(stream)
        call get_all_playlists(playlists, count)
        
        ! Build playlist cards
        cards_html = ''
        if (count == 0) then
            cards_html = '<div class="alert alert-info">No playlists available yet.</div>'
        else
            cards_html = '<div class="row row-cols-1 row-cols-md-2 row-cols-lg-3 g-4">'
            do i = 1, count
                write(id_str, '(I0)') playlists(i)%id
                call template_init()
                call template_set('id', trim(id_str))
                call template_set('name', trim(playlists(i)%name))
                call template_set('description', trim(playlists(i)%description))
                call template_render('template/partials/playlist_card.html', card_html, success)
                cards_html = trim(cards_html) // trim(card_html)
            end do
            cards_html = trim(cards_html) // '</div>'
        end if
        
        ! Render main template
        call template_init()
        call template_set('title', 'Music Player')
        call template_set('content', trim(cards_html))
        call template_render('template/index.html', html_output, success)
        
        if (success) then
            call output(html_output, stream)
        else
            ! Fallback if template not found
            call output('<!DOCTYPE html><html><head><title>Music Player</title>', stream)
            call output('<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/' // &
                       'bootstrap.min.css" rel="stylesheet"></head>', stream)
            call output('<body class="bg-body-secondary"><div class="container my-5">', stream)
            call output('<h1>Music Player</h1>', stream)
            call output(trim(cards_html), stream)
            call output('</div></body></html>', stream)
        end if
    end subroutine handle_public_index
    
    ! Handle public player page
    subroutine handle_public_player(id, stream)
        integer, intent(in) :: id
        type(c_ptr), intent(in) :: stream
        type(playlist_t) :: playlist
        type(song_t), save :: songs(100)
        integer :: count, i
        logical :: found, success
        character(len=16) :: pos_str
        character(len=65536) :: html_output, songs_html
        character(len=4096) :: song_html
        
        call get_playlist(id, playlist, found)
        call html_header(stream)
        
        if (.not. found) then
            call handle_404(stream)
            return
        end if
        
        call get_playlist_songs(id, songs, count)
        
        ! Build songs list
        songs_html = ''
        if (count == 0) then
            songs_html = '<li class="list-group-item text-muted">No songs in this playlist</li>'
        else
            do i = 1, count
                write(pos_str, '(I0)') i - 1
                call template_init()
                call template_set('index', trim(pos_str))
                call template_set('filename', trim(songs(i)%filename))
                call template_set('filepath', trim(songs(i)%filepath))
                call template_set('position', trim(pos_str))
                call template_set('title', trim(songs(i)%title))
                call template_render('template/partials/song_item.html', song_html, success)
                songs_html = trim(songs_html) // trim(song_html)
            end do
        end if
        
        ! Render main template
        call template_init()
        call template_set('playlist_name', trim(playlist%name))
        call template_set('songs_list', trim(songs_html))
        call template_render('template/player.html', html_output, success)
        
        call output(html_output, stream)
    end subroutine handle_public_player
    
    ! Handle admin login form
    subroutine handle_login_form(stream)
        type(c_ptr), intent(in) :: stream
        character(len=65536) :: html_output
        logical :: success
        
        call html_header(stream)
        
        call template_init()
        call template_set('error_message', '')
        call template_render('template/login.html', html_output, success)
        
        call output(html_output, stream)
    end subroutine handle_login_form
    
    ! Handle login POST
    subroutine handle_login_post(post_data, stream)
        character(len=*), intent(in) :: post_data
        type(c_ptr), intent(in) :: stream
        character(len=1024) :: username, password
        character(len=64) :: token
        character(len=256) :: cookie
        character(len=65536) :: html_output
        logical :: success
        
        username = parse_form_value(post_data, 'username')
        password = parse_form_value(post_data, 'password')
        
        if (validate_login(username, password)) then
            call create_session(token)
            cookie = get_session_cookie(token)
            call redirect_with_cookie('/admin', cookie, stream)
        else
            call html_header(stream)
            
            call template_init()
            call template_set('error_message', &
                '<div class="alert alert-danger mb-3">' // &
                '<i class="bi bi-exclamation-triangle"></i> Invalid username or password.' // &
                '</div>')
            call template_render('template/login.html', html_output, success)
            
            call output(html_output, stream)
        end if
    end subroutine handle_login_post
    
    ! Handle logout
    subroutine handle_logout(stream)
        type(c_ptr), intent(in) :: stream
        
        call destroy_session()
        call redirect_with_cookie('/', 'session_token=; Path=/; Max-Age=0', stream)
    end subroutine handle_logout
    
    ! Handle admin dashboard
    subroutine handle_admin_dashboard(stream)
        type(c_ptr), intent(in) :: stream
        type(playlist_t), save :: playlists(100)
        integer :: count, i, song_count
        character(len=16) :: id_str
        character(len=65536) :: html_output, table_html
        character(len=4096) :: row_html
        logical :: success
        
        call get_all_playlists(playlists, count)
        song_count = get_song_count()
        
        ! Build playlists table
        if (count == 0) then
            table_html = '<div class="alert alert-info">No playlists yet. Create your first playlist!</div>'
        else
            table_html = '<div class="table-responsive"><table class="table table-hover">' // &
                        '<thead class="table-dark"><tr>' // &
                        '<th>ID</th><th>Name</th><th>Description</th><th>Actions</th>' // &
                        '</tr></thead><tbody>'
            
            do i = 1, count
                write(id_str, '(I0)') playlists(i)%id
                call template_init()
                call template_set('id', trim(id_str))
                call template_set('name', trim(playlists(i)%name))
                call template_set('description', trim(playlists(i)%description))
                call template_render('template/partials/playlist_row.html', row_html, success)
                table_html = trim(table_html) // trim(row_html)
            end do
            
            table_html = trim(table_html) // '</tbody></table></div>'
        end if
        
        ! Render main template
        call html_header(stream)
        call template_init()
        call template_set_int('playlist_count', count)
        call template_set_int('song_count', song_count)
        call template_set('playlists_table', trim(table_html))
        call template_render('template/admin_dashboard.html', html_output, success)
        
        call output(html_output, stream)
    end subroutine handle_admin_dashboard
    
    ! Handle playlist form (new/edit)
    subroutine handle_playlist_form(id, stream)
        integer, intent(in) :: id
        type(c_ptr), intent(in) :: stream
        type(playlist_t) :: playlist
        type(song_t), save :: songs(100)
        integer :: count, i
        logical :: found, success
        character(len=16) :: id_str, song_id_str
        character(len=256) :: form_title, action_url
        character(len=65536) :: html_output, songs_section
        character(len=4096) :: song_html
        
        if (id > 0) then
            call get_playlist(id, playlist, found)
            if (.not. found) then
                call handle_404(stream)
                return
            end if
            call get_playlist_songs(id, songs, count)
            form_title = 'Edit Playlist'
            write(id_str, '(I0)') id
            action_url = '/admin/playlist/' // trim(id_str) // '/edit'
        else
            playlist%name = ''
            playlist%description = ''
            form_title = 'New Playlist'
            action_url = '/admin/playlist/new'
            count = 0
        end if
        
        ! Build songs section for edit mode
        songs_section = ''
        if (id > 0) then
            write(id_str, '(I0)') id
            
            ! Songs list
            songs_section = '<div class="card mb-4">' // &
                '<div class="card-header d-flex justify-content-between align-items-center">' // &
                '<h5 class="mb-0"><i class="bi bi-list-ol"></i> Playlist Songs</h5>' // &
                '<small class="text-muted">Drag to reorder</small></div>' // &
                '<ul class="list-group list-group-flush" id="sortable-songs" data-playlist-id="' // trim(id_str) // '">'
            
            if (count == 0) then
                songs_section = trim(songs_section) // &
                    '<li class="list-group-item text-muted">No songs in this playlist</li>'
            else
                do i = 1, count
                    write(song_id_str, '(I0)') songs(i)%id
                    call template_init()
                    call template_set('song_id', trim(song_id_str))
                    call template_set('playlist_id', trim(id_str))
                    call template_set('title', trim(songs(i)%title))
                    call template_render('template/partials/sortable_song.html', song_html, success)
                    songs_section = trim(songs_section) // trim(song_html)
                end do
            end if
            
            songs_section = trim(songs_section) // '</ul></div>'
            
            ! Upload form
            songs_section = trim(songs_section) // &
                '<div class="card"><div class="card-header">' // &
                '<h5 class="mb-0"><i class="bi bi-upload"></i> Upload Songs</h5></div>' // &
                '<div class="card-body">' // &
                '<form method="POST" action="/admin/upload" enctype="multipart/form-data" id="uploadForm">' // &
                '<input type="hidden" name="playlist_id" value="' // trim(id_str) // '">' // &
                '<div class="mb-3">' // &
                '<input type="file" class="form-control" name="mp3files[]" accept=".mp3" multiple required>' // &
                '<div class="form-text">Select one or more MP3 files to upload</div></div>' // &
                '<button type="submit" class="btn btn-success">' // &
                '<i class="bi bi-cloud-upload"></i> Upload</button>' // &
                '</form></div></div>'
        end if
        
        ! Render main template
        call html_header(stream)
        call template_init()
        call template_set('form_title', trim(form_title))
        call template_set('form_action', trim(action_url))
        call template_set('playlist_name', trim(playlist%name))
        call template_set('playlist_description', trim(playlist%description))
        call template_set('songs_section', trim(songs_section))
        call template_render('template/admin_playlist_form.html', html_output, success)
        
        call output(html_output, stream)
    end subroutine handle_playlist_form
    
    ! Handle playlist create
    subroutine handle_playlist_create(post_data, stream)
        character(len=*), intent(in) :: post_data
        type(c_ptr), intent(in) :: stream
        character(len=1024) :: name, description
        integer :: id
        character(len=16) :: id_str
        
        name = parse_form_value(post_data, 'name')
        description = parse_form_value(post_data, 'description')
        
        id = create_playlist(name, description)
        
        if (id > 0) then
            write(id_str, '(I0)') id
            call redirect('/admin/playlist/' // trim(id_str) // '/edit', stream)
        else
            call redirect('/admin', stream)
        end if
    end subroutine handle_playlist_create
    
    ! Handle playlist update
    subroutine handle_playlist_update(id, post_data, stream)
        integer, intent(in) :: id
        character(len=*), intent(in) :: post_data
        type(c_ptr), intent(in) :: stream
        character(len=1024) :: name, description
        logical :: success
        character(len=16) :: id_str
        
        name = parse_form_value(post_data, 'name')
        description = parse_form_value(post_data, 'description')
        
        call update_playlist(id, name, description, success)
        
        write(id_str, '(I0)') id
        call redirect('/admin/playlist/' // trim(id_str) // '/edit', stream)
    end subroutine handle_playlist_update
    
    ! Handle playlist delete
    subroutine handle_playlist_delete(id, stream)
        integer, intent(in) :: id
        type(c_ptr), intent(in) :: stream
        logical :: success
        
        call delete_playlist(id, success)
        call redirect('/admin', stream)
    end subroutine handle_playlist_delete
    
    ! Handle file upload - parse multipart form data
    subroutine handle_file_upload(in_stream, content_type, content_length, out_stream)
        type(c_ptr), intent(in) :: in_stream, out_stream
        character(len=*), intent(in) :: content_type
        integer, intent(in) :: content_length
        
        character(len=256) :: boundary, filename, safe_filename, title
        character(len=512) :: filepath
        character(len=16) :: playlist_id_str
        integer :: playlist_id, song_id, uploaded, next_pos
        integer :: i, boundary_len, state
        integer :: file_unit, line_start, ios
        integer(c_int) :: ich
        logical :: in_file, success, has_filename
        integer :: buf_pos, header_end
        integer(1) :: byte_out  ! For proper binary byte writing
        
        ! Use SAVE to put large arrays in static storage (prevent stack overflow)
        integer, dimension(8192), save :: byte_buf
        character(len=8192), save :: line_buf
        
        ! Constants for state machine
        integer, parameter :: STATE_BOUNDARY = 1
        integer, parameter :: STATE_HEADERS = 2
        integer, parameter :: STATE_DATA = 3
        integer :: bytes_written, debug_unit
        
        uploaded = 0
        playlist_id = 0
        file_unit = 50
        debug_unit = 99
        bytes_written = 0
        
        ! Open debug log
        open(unit=debug_unit, file='debug_upload.log', status='replace', action='write')
        write(debug_unit, '(A,I0)') 'Content-Length: ', content_length
        write(debug_unit, '(A,A)') 'Content-Type: ', trim(content_type)
        
        ! Extract boundary from content type
        i = index(content_type, 'boundary=')
        if (i == 0) then
            write(debug_unit, '(A)') 'ERROR: No boundary found!'
            close(debug_unit)
            call json_header(out_stream)
            call output('{"status": "error", "message": "No boundary in content type"}', out_stream)
            return
        end if
        boundary = '--' // trim(content_type(i+9:))
        boundary_len = len_trim(boundary)
        write(debug_unit, '(A,A)') 'Boundary: ', trim(boundary)
        
        ! Read multipart data byte by byte
        state = STATE_BOUNDARY
        in_file = .false.
        has_filename = .false.
        buf_pos = 0
        filename = ''
        byte_buf = 0
        
        do i = 1, content_length
            ich = FCGX_GetChar(in_stream)
            if (ich < 0) exit
            
            if (state == STATE_DATA .and. in_file) then
                ! Write file data directly (binary safe)
                buf_pos = buf_pos + 1
                if (buf_pos <= size(byte_buf)) then
                    byte_buf(buf_pos) = iand(int(ich), 255)
                end if
                
                ! Check if we hit a newline - potential boundary, OR buffer nearly full
                if (ich == 10 .or. buf_pos >= 7800) then  ! LF or buffer almost full
                    ! Convert buffer to string just for boundary check
                    call bytes_to_string(byte_buf, buf_pos, line_buf)
                    
                    ! Only check for boundary on newlines
                    header_end = 0
                    if (ich == 10 .and. buf_pos > boundary_len .and. buf_pos <= len(line_buf)) then
                        header_end = index(line_buf(1:buf_pos), trim(boundary))
                    end if
                    
                    if (header_end > 0) then
                        ! Found boundary - write everything before it (skip CRLF)
                        if (header_end > 3) then
                            do line_start = 1, header_end - 3
                                byte_out = int(iand(byte_buf(line_start), 255), 1)
                                write(file_unit, iostat=ios) byte_out
                                bytes_written = bytes_written + 1
                            end do
                        end if
                        write(debug_unit, '(A,I0)') 'Closing file, bytes_written: ', bytes_written
                        close(file_unit, iostat=ios)
                        in_file = .false.
                        
                        ! Validate and add to database
                        if (len_trim(filename) > 0 .and. playlist_id > 0) then
                            write(debug_unit, '(A,A)') 'Validating: ', trim(filepath)
                            if (validate_mp3_file(trim(filepath))) then
                                write(debug_unit, '(A)') 'MP3 validation PASSED'
                                title = get_title_from_filename(filename)
                                song_id = create_song(trim(filename), trim(title), '/mp3/' // trim(safe_filename))
                                if (song_id > 0) then
                                    next_pos = get_next_position(playlist_id)
                                    call add_song_to_playlist(playlist_id, song_id, next_pos, success)
                                    if (success) uploaded = uploaded + 1
                                end if
                            else
                                write(debug_unit, '(A)') 'MP3 validation FAILED - deleting file'
                                call delete_file(trim(filepath))
                            end if
                        end if
                        
                        filename = ''
                        state = STATE_HEADERS
                        buf_pos = 0
                        byte_buf = 0
                    else
                        ! Not a boundary (or buffer full), write all buffered data to file
                        do line_start = 1, buf_pos
                            byte_out = int(iand(byte_buf(line_start), 255), 1)
                            write(file_unit, iostat=ios) byte_out
                            bytes_written = bytes_written + 1
                        end do
                        buf_pos = 0
                        byte_buf = 0
                    end if
                end if
            else
                ! Accumulate line in byte buffer
                buf_pos = buf_pos + 1
                if (buf_pos <= size(byte_buf)) then
                    byte_buf(buf_pos) = iand(int(ich), 255)
                end if
                
                ! Check for line end
                if (ich == 10) then  ! LF
                    ! Convert to string for header parsing (only ASCII parts matter)
                    call bytes_to_string(byte_buf, buf_pos, line_buf)
                    
                    if (state == STATE_BOUNDARY) then
                        if (index(line_buf(1:min(buf_pos,len(line_buf))), trim(boundary)) > 0) then
                            state = STATE_HEADERS
                        end if
                    else if (state == STATE_HEADERS) then
                        if (buf_pos <= 2) then
                            ! Empty line - end of headers
                            if (len_trim(filename) > 0) then
                                ! Create unique filename
                                safe_filename = make_safe_filename(filename)
                                filepath = 'public/mp3/' // trim(safe_filename)
                                write(debug_unit, '(A,A)') 'Opening file: ', trim(filepath)
                                open(unit=file_unit, file=trim(filepath), status='replace', &
                                     access='stream', form='unformatted', iostat=ios)
                                if (ios == 0) then
                                    in_file = .true.
                                    bytes_written = 0
                                    write(debug_unit, '(A)') 'File opened successfully'
                                else
                                    write(debug_unit, '(A,I0)') 'ERROR opening file, iostat=', ios
                                end if
                                state = STATE_DATA
                            else
                                state = STATE_DATA
                            end if
                        else
                            ! Check for filename - just detect presence, use timestamp-based name
                            header_end = index(line_buf(1:min(buf_pos,len(line_buf))), 'filename="')
                            if (header_end > 0) then
                                ! Generate timestamp-based filename to avoid UTF-8 issues
                                call generate_upload_filename(filename)
                                has_filename = .true.
                            end if
                            ! Check for playlist_id
                            header_end = index(line_buf(1:min(buf_pos,len(line_buf))), 'name="playlist_id"')
                            if (header_end > 0) then
                                state = STATE_DATA
                                in_file = .false.
                            end if
                        end if
                    else if (state == STATE_DATA .and. .not. in_file) then
                        ! Reading form field value (like playlist_id)
                        if (index(line_buf(1:min(buf_pos,len(line_buf))), trim(boundary)) > 0) then
                            state = STATE_HEADERS
                        else if (buf_pos > 2) then
                            ! This is the playlist_id value
                            read(line_buf(1:buf_pos-2), *, iostat=ios) playlist_id
                        end if
                    end if
                    buf_pos = 0
                    byte_buf = 0
                end if
            end if
        end do
        
        ! Close file if still open
        if (in_file) then
            close(file_unit, iostat=ios)
            ! Process last file
            if (len_trim(filename) > 0 .and. playlist_id > 0) then
                if (validate_mp3_file(trim(filepath))) then
                    title = get_title_from_filename(filename)
                    song_id = create_song(trim(filename), trim(title), '/mp3/' // trim(safe_filename))
                    if (song_id > 0) then
                        next_pos = get_next_position(playlist_id)
                        call add_song_to_playlist(playlist_id, song_id, next_pos, success)
                        if (success) uploaded = uploaded + 1
                    end if
                else
                    call delete_file(trim(filepath))
                end if
            end if
        end if
        
        ! Redirect back to playlist edit
        if (playlist_id > 0) then
            write(playlist_id_str, '(I0)') playlist_id
            write(debug_unit, '(A,I0)') 'Redirecting to playlist: ', playlist_id
            write(debug_unit, '(A,I0)') 'Total uploaded: ', uploaded
            close(debug_unit)
            call redirect('/admin/playlist/' // trim(playlist_id_str) // '/edit', out_stream)
        else
            write(debug_unit, '(A)') 'Redirecting to admin'
            close(debug_unit)
            call redirect('/admin', out_stream)
        end if
    end subroutine handle_file_upload
    
    ! Convert byte array to string (binary safe)
    subroutine bytes_to_string(bytes, length, str)
        integer, intent(in) :: bytes(:), length
        character(len=*), intent(out) :: str
        integer :: i, b, max_len
        
        str = ''
        ! Limit to smallest of: length, str size, bytes array size
        max_len = min(length, len(str), size(bytes))
        if (max_len <= 0) return
        
        do i = 1, max_len
            b = iand(bytes(i), 255)
            if (b >= 32 .and. b < 127) then
                str(i:i) = achar(b)
            else if (b == 10 .or. b == 13) then
                str(i:i) = achar(b)  ! Keep CR/LF
            else
                str(i:i) = '_'  ! Replace other bytes
            end if
        end do
    end subroutine bytes_to_string
    
    ! Get next position in playlist
    function get_next_position(playlist_id) result(pos)
        integer, intent(in) :: playlist_id
        integer :: pos
        
        ! Use get_song_count_in_playlist from db_module
        pos = get_song_count_in_playlist(playlist_id) + 1
    end function get_next_position
    
    ! Generate unique filename for upload (avoids UTF-8 issues)
    subroutine generate_upload_filename(filename)
        character(len=*), intent(out) :: filename
        integer :: values(8)
        integer, save :: counter = 0
        
        call date_and_time(values=values)
        counter = counter + 1
        
        write(filename, '(A,I4.4,I2.2,I2.2,A,I2.2,I2.2,I2.2,A,I4.4,A)') &
            'upload_', values(1), values(2), values(3), '_', &
            values(5), values(6), values(7), '_', counter, '.mp3'
    end subroutine generate_upload_filename
    
    ! Make filename safe for filesystem (handles UTF-8)
    function make_safe_filename(filename) result(safe)
        character(len=*), intent(in) :: filename
        character(len=256) :: safe
        integer :: i, j, byte_val
        character(len=1) :: ch
        
        safe = ''
        j = 0
        do i = 1, len_trim(filename)
            if (j >= 200) exit  ! Limit output length
            ch = filename(i:i)
            byte_val = ichar(ch)
            
            ! Handle UTF-8 multi-byte characters (high bit set = > 127 or < 0 as signed)
            ! Also handle spaces and special characters
            if (byte_val < 0 .or. byte_val > 127) then
                ! UTF-8 continuation byte or high byte - skip or replace
                j = j + 1
                safe(j:j) = '_'
            else if ((ch >= 'a' .and. ch <= 'z') .or. &
                     (ch >= 'A' .and. ch <= 'Z') .or. &
                     (ch >= '0' .and. ch <= '9') .or. &
                     ch == '-' .or. ch == '_' .or. ch == '.') then
                j = j + 1
                safe(j:j) = ch
            else
                ! Replace other characters with underscore
                j = j + 1
                safe(j:j) = '_'
            end if
        end do
        
        ! Ensure we have a valid filename
        if (j == 0) then
            safe = 'uploaded_file.mp3'
        end if
    end function make_safe_filename
    
    ! Delete a file
    subroutine delete_file(filepath)
        character(len=*), intent(in) :: filepath
        integer :: ios, unit_num
        logical :: exists
        
        unit_num = 51
        inquire(file=filepath, exist=exists)
        if (exists) then
            open(unit=unit_num, file=filepath, status='old', iostat=ios)
            if (ios == 0) close(unit_num, status='delete')
        end if
    end subroutine delete_file
    
    ! Handle song reorder
    subroutine handle_song_reorder(id, post_data, stream)
        integer, intent(in) :: id
        character(len=*), intent(in) :: post_data
        type(c_ptr), intent(in) :: stream
        integer :: playlist_id, data_len
        
        ! Use arguments to suppress warnings (actual reorder logic would use these)
        playlist_id = id
        data_len = len_trim(post_data)
        
        call json_header(stream)
        call output('{"status": "ok"}', stream)
    end subroutine handle_song_reorder
    
    ! Handle remove song from playlist
    subroutine handle_remove_song_from_playlist(path, stream)
        character(len=*), intent(in) :: path
        type(c_ptr), intent(in) :: stream
        integer :: song_id, playlist_id, pos1, pos2
        logical :: success
        character(len=16) :: id_str
        
        pos1 = index(path, '/admin/song/') + 12
        pos2 = index(path(pos1:), '/') + pos1 - 2
        read(path(pos1:pos2), *, err=30) song_id
        
        pos1 = index(path, '/remove/') + 8
        read(path(pos1:), *, err=30) playlist_id
        
        call remove_song_from_playlist(playlist_id, song_id, success)
        
        write(id_str, '(I0)') playlist_id
        call redirect('/admin/playlist/' // trim(id_str) // '/edit', stream)
        return
        
        30 call redirect('/admin', stream)
    end subroutine handle_remove_song_from_playlist
    
    ! Handle 404 Not Found
    subroutine handle_404(stream)
        type(c_ptr), intent(in) :: stream
        integer(c_int) :: rc
        character(len=65536) :: html_output
        logical :: success
        
        rc = FCGX_PutS('Status: 404 Not Found' // char(13) // char(10) // c_null_char, stream)
        call html_header(stream)
        
        call template_init()
        call template_render('template/404.html', html_output, success)
        
        if (success) then
            call output(html_output, stream)
        else
            call output('<h1>404 - Page Not Found</h1>', stream)
        end if
    end subroutine handle_404

end program fortran_fcgi
