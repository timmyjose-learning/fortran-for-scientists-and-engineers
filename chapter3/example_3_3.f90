program example_3_3
   implicit none

   real :: x, y
   real :: res

   write (*, *) 'Enter the value of x and y'
   read (*, *) x, y

   if (x >= 0.0) then
      if (y >= 0.0) then
         res = x + y
      else
         res = x + y**2
      end if
   else if (y >= 0.0) then
      res = x**2 + y
   else
      res = x**2 + y**2
   end if

   write (*, *) 'result = ', res
end program example_3_3
