program read_file_and_print_contents
   implicit none

   character(len=256) :: filename
   integer :: iostat
   character(len=256) :: iomsg

   write (*, *) 'Enter the filename'
   read (*, *) filename

   call print_file_contents(filename, iostat, iomsg)

   ! iostat will be -1 in case of normal eof, so check for actual file read errors (iostat > 0)
   if (iostat > 0) then
      write (*, *) 'Error: ', iomsg
   end if

contains
   subroutine print_file_contents(file, stat, msg)
      implicit none

      character(len=*), intent(in) :: file
      character(len=*), intent(out) :: msg
      integer :: unit
      integer, intent(out) :: stat
      character(len=1024) :: line

      open (newunit=unit, file=file, status='old', action='read', iostat=stat, iomsg=msg)

      if (stat == 0) then
         do
            read (unit, '(A)', iostat=stat) line

            if (stat /= 0) then
               exit
            end if
            write (*, *) trim(line)
         end do

         close (unit=unit)
      end if
   end subroutine print_file_contents
end program read_file_and_print_contents
