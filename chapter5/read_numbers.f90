program read_numbers
   implicit none

   character(len=20) :: filename
   integer :: nvals = 0, n, sum = 0
   integer :: unit, io_status
   character(len=256) :: err_msg

   write (*, *) 'Enter filename'
   read (*, *) filename

   unit = 1

   open (unit=unit, file=filename, status='old', action='read', iostat=io_status, iomsg=err_msg)

   ! if iostat > 0 -> file read/write error
   ! if iostat = 0 -> no read error
   ! if iostate < 0 -> EOF
   if (io_status == 0) then
      do
         read (unit, *, iostat=io_status) n

         if (io_status /= 0) then
            exit
         end if

         nvals = nvals + 1
         sum = sum + n
      end do

      if (io_status > 0) then
         write (*, *) 'Error while reading contents of file: ', err_msg
         close (unit=unit)
         stop
      end if

      write (*, *) 'Sum = ', sum
      close (unit=unit)
   else
      write (*, *) 'Errror while opening file: ', err_msg
   end if
end program read_numbers

