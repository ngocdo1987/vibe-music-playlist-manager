! env_module.f90 - Environment file parser module
! Reads configuration from .env file

module env_module
    implicit none
    private
    
    ! Public variables to hold config values
    character(len=256), public :: admin_user = ''
    character(len=256), public :: admin_pass = ''
    character(len=256), public :: session_secret = ''
    
    public :: load_env
    
contains

    subroutine load_env()
        ! Load environment variables from .env file
        character(len=512) :: line, key
        character(len=256) :: value
        integer :: ios, eq_pos, unit_num
        
        unit_num = 20
        
        open(unit=unit_num, file='.env', status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Warning: Could not open .env file'
            return
        end if
        
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            ! Skip empty lines and comments
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#') cycle
            
            ! Find the equals sign
            eq_pos = index(line, '=')
            if (eq_pos == 0) cycle
            
            ! Extract key and value
            key = trim(adjustl(line(1:eq_pos-1)))
            value = trim(adjustl(line(eq_pos+1:)))
            
            ! Set the appropriate variable
            select case (trim(key))
                case ('ADMIN_USER')
                    admin_user = value
                case ('ADMIN_PASS')
                    admin_pass = value
                case ('SESSION_SECRET')
                    session_secret = value
            end select
        end do
        
        close(unit_num)
    end subroutine load_env

end module env_module
