program add_nums
   implicit none

   integer :: x, y, z, sum = 0
   integer :: unit = 1
   integer :: io_err
   character(len=256) :: err_msg

   open (unit=unit, file='numbers.dat', action='read', status='old', iostat=io_err, iomsg=err_msg)

   if (io_err /= 0) then
      write (*, *) 'Error while opening file: ', err_msg
      stop
   end if

   read (unit, *) x, y, z
   sum = x + y + z
   write (*, *) 'The sum of ', x, ', ', y, ', and ', z, ' is = ', sum

   close (unit=unit)

end program add_nums
