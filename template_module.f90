! template_module.f90 - HTML template rendering module
! Reads HTML template files and replaces placeholders like {{variable}}

module template_module
    implicit none
    private
    
    ! Maximum template size
    integer, parameter :: MAX_TEMPLATE_SIZE = 65536
    
    ! Variable storage for template rendering
    integer, parameter :: MAX_VARS = 50
    character(len=64), dimension(MAX_VARS), save :: var_names
    character(len=4096), dimension(MAX_VARS), save :: var_values
    integer, save :: var_count = 0
    
    public :: template_init, template_set, template_set_int
    public :: template_render, template_clear
    
contains

    ! Initialize/clear template variables
    subroutine template_init()
        var_count = 0
        var_names = ''
        var_values = ''
    end subroutine template_init
    
    ! Alias for template_init
    subroutine template_clear()
        call template_init()
    end subroutine template_clear
    
    ! Set a template variable (string)
    subroutine template_set(name, value)
        character(len=*), intent(in) :: name, value
        
        if (var_count < MAX_VARS) then
            var_count = var_count + 1
            var_names(var_count) = trim(name)
            var_values(var_count) = trim(value)
        end if
    end subroutine template_set
    
    ! Set a template variable (integer)
    subroutine template_set_int(name, value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(len=16) :: str_val
        
        write(str_val, '(I0)') value
        call template_set(name, trim(str_val))
    end subroutine template_set_int
    
    ! Render a template file with variable substitution
    subroutine template_render(template_path, output_str, success)
        character(len=*), intent(in) :: template_path
        character(len=*), intent(out) :: output_str
        logical, intent(out) :: success
        
        character(len=MAX_TEMPLATE_SIZE) :: template_content
        character(len=MAX_TEMPLATE_SIZE) :: result
        integer :: unit_num, ios, i
        character(len=512) :: line
        
        success = .false.
        output_str = ''
        template_content = ''
        unit_num = 40
        
        ! Read template file
        open(unit=unit_num, file=template_path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            output_str = '<!-- Template not found: ' // trim(template_path) // ' -->'
            return
        end if
        
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            template_content = trim(template_content) // trim(line) // char(10)
        end do
        
        close(unit_num)
        
        ! Replace variables
        result = template_content
        do i = 1, var_count
            call replace_var(result, var_names(i), var_values(i))
        end do
        
        output_str = trim(result)
        success = .true.
    end subroutine template_render
    
    ! Replace all occurrences of {{name}} with value
    subroutine replace_var(str, name, value)
        character(len=*), intent(inout) :: str
        character(len=*), intent(in) :: name, value
        character(len=MAX_TEMPLATE_SIZE) :: result, temp
        character(len=128) :: placeholder
        integer :: pos, placeholder_len
        
        placeholder = '{{' // trim(name) // '}}'
        placeholder_len = len_trim(placeholder)
        result = str
        
        do
            pos = index(result, trim(placeholder))
            if (pos == 0) exit
            
            temp = result(1:pos-1) // trim(value) // result(pos+placeholder_len:)
            result = temp
        end do
        
        str = result
    end subroutine replace_var

end module template_module
