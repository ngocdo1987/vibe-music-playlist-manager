! mp3_module.f90 - MP3 file validation and handling module
! Validates MP3 files by checking magic bytes and handles file operations

module mp3_module
    implicit none
    private
    
    public :: validate_mp3_file, save_uploaded_file, get_title_from_filename
    
contains

    ! Validate MP3 file by checking magic bytes
    ! MP3 files start with ID3 tag (49 44 33) or frame sync (FF FB/FA/F3/F2)
    function validate_mp3_file(filepath) result(valid)
        character(len=*), intent(in) :: filepath
        logical :: valid
        integer :: unit_num, ios
        integer :: byte1, byte2, byte3
        character(len=1) :: ch1, ch2, ch3
        
        valid = .false.
        unit_num = 30
        
        open(unit=unit_num, file=filepath, status='old', access='stream', &
             form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        ! Read first 3 bytes one at a time
        read(unit_num, iostat=ios) ch1
        if (ios /= 0) then
            close(unit_num)
            return
        end if
        read(unit_num, iostat=ios) ch2
        if (ios /= 0) then
            close(unit_num)
            return
        end if
        read(unit_num, iostat=ios) ch3
        close(unit_num)
        
        if (ios /= 0) return
        
        ! Get byte values using iachar for binary-safe conversion (0-255)
        byte1 = iachar(ch1)
        byte2 = iachar(ch2)
        byte3 = iachar(ch3)
        
        ! Check for ID3 tag (ID3v2) - 'I'=73, 'D'=68, '3'=51
        if (byte1 == 73 .and. byte2 == 68 .and. byte3 == 51) then
            valid = .true.
            return
        end if
        
        ! Check for MP3 frame sync (0xFF followed by 0xFB, 0xFA, 0xF3, or 0xF2)
        if (byte1 == 255) then  ! 0xFF
            if (byte2 == 251 .or. byte2 == 250 .or. &  ! 0xFB, 0xFA
                byte2 == 243 .or. byte2 == 242 .or. &  ! 0xF3, 0xF2
                byte2 == 227 .or. byte2 == 226) then   ! 0xE3, 0xE2 (MPEG 2.5)
                valid = .true.
            end if
        end if
    end function validate_mp3_file
    
    ! Save uploaded file to mp3 directory
    subroutine save_uploaded_file(source_data, filename, success)
        character(len=*), intent(in) :: source_data, filename
        logical, intent(out) :: success
        character(len=512) :: dest_path
        integer :: unit_num, ios
        
        success = .false.
        unit_num = 31
        dest_path = 'mp3/' // trim(filename)
        
        open(unit=unit_num, file=dest_path, status='replace', access='stream', &
             form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        write(unit_num, iostat=ios) source_data
        close(unit_num)
        
        if (ios == 0) success = .true.
    end subroutine save_uploaded_file
    
    ! Extract title from filename (remove extension)
    function get_title_from_filename(filename) result(title)
        character(len=*), intent(in) :: filename
        character(len=256) :: title
        integer :: i, dot_pos, title_len
        
        title = ''
        title_len = 0
        
        ! Find the last dot for extension removal
        dot_pos = 0
        do i = len_trim(filename), 1, -1
            if (filename(i:i) == '.') then
                dot_pos = i
                exit
            end if
        end do
        
        ! Copy characters, replacing underscores with spaces
        if (dot_pos > 1) then
            do i = 1, dot_pos - 1
                title_len = title_len + 1
                if (title_len > 256) exit
                if (filename(i:i) == '_') then
                    title(title_len:title_len) = ' '
                else
                    title(title_len:title_len) = filename(i:i)
                end if
            end do
        else
            do i = 1, len_trim(filename)
                title_len = title_len + 1
                if (title_len > 256) exit
                if (filename(i:i) == '_') then
                    title(title_len:title_len) = ' '
                else
                    title(title_len:title_len) = filename(i:i)
                end if
            end do
        end if
        
        ! If title is empty, use a default
        if (title_len == 0) then
            title = 'Untitled'
        end if
    end function get_title_from_filename

end module mp3_module
