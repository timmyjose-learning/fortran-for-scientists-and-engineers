program stats_1
   implicit none

   integer :: n = 0
   real :: x = 0.0
   real :: sum_x = 0.0, sum_x2 = 0.0
   real :: x_bar = 0.0, std_dev = 0.0

   do
      write (*, *) 'Enter x'
      read (*, *) x

      if (x < 0.0) then
         exit
      end if

      sum_x = sum_x + x
      sum_x2 = sum_x2 + x**2
      n = n + 1
   end do

   if (n < 2) then
      write (*, *) 'We need at least 2 data points'
   else

      x_bar = sum_x/real(n)
      std_dev = sqrt((real(n)*sum_x2 - sum_x**2)/(real(n)*real(n - 1)))

      write (*, *) 'mean = ', x_bar
      write (*, *) 'standard deviation = ', std_dev
      write (*, *) 'The number of data points is ', n
   end if
end program stats_1
