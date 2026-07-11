program my_first_program
   implicit none

   integer :: i, j, k

   write (*, *) 'Enter two numbers'
   read (*, *) i, j

   k = i*j
   write (*, *) 'Result = ', k

   stop
end program my_first_program
