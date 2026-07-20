program sqrt_of_array
   implicit none

   integer, parameter :: ARR_SIZE = 5
   real, dimension(ARR_SIZE) ::a = (/1.0, 2.0, 3.0, 4.0, 5.0/), b

   b = sqrt(a)
   write (*, *) b
end program sqrt_of_array
