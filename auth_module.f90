! auth_module.f90 - Authentication and session management module
! Handles admin login, session tokens, and authentication checks

module auth_module
    use env_module
    implicit none
    private
    
    ! Session storage (simple in-memory for this demo)
    character(len=64), save :: active_session_token = ''
    logical, save :: session_active = .false.
    
    public :: auth_init, validate_login, create_session, check_session
    public :: destroy_session, generate_token, get_session_cookie
    
contains

    ! Initialize authentication module
    subroutine auth_init()
        call load_env()
    end subroutine auth_init
    
    ! Validate login credentials
    function validate_login(username, password) result(valid)
        character(len=*), intent(in) :: username, password
        logical :: valid
        
        valid = .false.
        
        ! Compare with environment variables
        if (trim(username) == trim(admin_user) .and. &
            trim(password) == trim(admin_pass)) then
            valid = .true.
        end if
    end function validate_login
    
    ! Generate a simple session token
    function generate_token() result(token)
        character(len=64) :: token
        integer :: i
        real :: r
        character(len=62), parameter :: chars = &
            'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
        
        token = ''
        call random_seed()
        
        do i = 1, 32
            call random_number(r)
            token = trim(token) // chars(int(r * 62) + 1:int(r * 62) + 1)
        end do
    end function generate_token
    
    ! Create a new session
    subroutine create_session(token)
        character(len=64), intent(out) :: token
        
        token = generate_token()
        active_session_token = token
        session_active = .true.
    end subroutine create_session
    
    ! Check if session token is valid
    function check_session(token) result(valid)
        character(len=*), intent(in) :: token
        logical :: valid
        
        valid = .false.
        
        if (session_active .and. len_trim(token) > 0) then
            if (trim(token) == trim(active_session_token)) then
                valid = .true.
            end if
        end if
    end function check_session
    
    ! Destroy current session
    subroutine destroy_session()
        active_session_token = ''
        session_active = .false.
    end subroutine destroy_session
    
    ! Get Set-Cookie header value for session
    function get_session_cookie(token) result(cookie)
        character(len=*), intent(in) :: token
        character(len=256) :: cookie
        
        cookie = 'session_token=' // trim(token) // '; Path=/; HttpOnly'
    end function get_session_cookie

end module auth_module
