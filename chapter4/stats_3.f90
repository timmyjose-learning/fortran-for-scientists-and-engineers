program stats_3
   implicit none

   integer :: n, idx
   real :: x = 0.0, sum_x = 0.0, sum_x2 = 0.0
   real :: std_dev, x_bar

   do
      write (*, *) 'How many entries?'
      read (*, *) n

      if (n < 2) then
         write (*, *) 'We need at least 2 data points'
         cycle
      else
         exit
      end if
   end do

   do idx = 1, n
      write (*, *) 'Enter a value'
      read (*, *) x

      sum_x = sum_x + x
      sum_x2 = sum_x2 + x**2
   end do

   x_bar = sum_x/real(n)
   std_dev = sqrt((real(n)*sum_x2 - sum_x**2)/(real(n)*real(n - 1)))

   write (*, *) 'mean = ', x_bar
   write (*, *) 'standard deviation = ', std_dev
end program stats_3
