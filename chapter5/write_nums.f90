program write_nums
   implicit none

   integer :: x, y, z
   integer :: unit = 1
   integer :: io_err
   character(len=256) :: err_msg

   write (*, *) 'Enter 3 numbers'
   read (*, *) x, y, z

   open (unit=unit, file='numbers.out', status='replace', action='write', iostat=io_err, iomsg=err_msg)

   if (io_err /= 0) then
      write (*, *) 'Error while writing to file: ', err_msg
      stop
   end if

   write (unit, *) x, y, z

   close (unit=unit)
end program write_nums
