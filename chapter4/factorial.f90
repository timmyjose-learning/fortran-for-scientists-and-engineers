program factorial
   implicit none

   integer :: n, d
   integer :: n_fact = 1

   do
      write (*, *) 'Enter a non-negatve integer'
      read (*, *) n

      if (n < 0) then
         cycle
      else
         exit
      end if
   end do

   do d = 1, n
      n_fact = n_fact*d
   end do

   write (*, *) 'The factorial of ', n, ' is ', n_fact
end program factorial

