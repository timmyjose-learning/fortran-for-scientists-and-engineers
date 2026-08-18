program check_exists_before_writing
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      character(len=512) :: filename
      logical :: exist
      integer :: unit
      integer :: iostat
      character(len=512) :: iomsg

      do
         write (output_unit, *) 'What is the filename?'
         read (input_unit, *) filename

         inquire (file=filename, exist=exist)

         if (.not. exist) then
            open(newunit=unit, file=filename, status='new', action='write', iostat=iostat, iomsg=iomsg)
            if (iostat /= 0) then
               error stop 'error opening file for write: ' // iomsg
            end if

            write (unit, '(A,I5)') 'Hello from Fortran', 2026

            close(unit, iostat=iostat, iomsg=iomsg)
            if (iostat /= 0) then
               write (error_unit, *) 'Error while closing file: ' // iomsg
            end if
            exit
         else
            block
               character(len=64) :: choice

               write (output_unit, *) 'File already exists. Enter a new filename'
               write (output_unit, *) 'Would you like to enter a new filename? [yes/no]'
               read (input_unit, *) choice

               call ucase(choice)

               select case(choice)
                case ('Y', 'YES', 'YEAH', 'YUP', 'YEP')
                  cycle
                case default
                  write (output_unit, *) 'Adios!'
                  exit
               end select
            end block
         end if
      end do
   end subroutine run_app

   subroutine ucase(char)
      implicit none

      integer, parameter :: GAP = 32
      character(len=*), intent(inout) :: char
      integer :: i

      do i = 1, len(char)
         if (is_lower(char(i:i))) then
            char(i:i) = achar(iachar(char(i:i)) - GAP)
         end if
      end do
   end subroutine ucase

   logical function is_lower(char)
      implicit none

      character(len=1), intent(in) :: char

      if (char >= 'a' .and. char <= 'z') then
         is_lower = .true.
      else
         is_lower = .false.
      end if
   end function is_lower
end program check_exists_before_writing