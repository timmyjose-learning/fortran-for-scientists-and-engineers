program mult_array_by_scala
   implicit none

   integer, parameter :: ARR_SIZE = 5
   integer, dimension(ARR_SIZE) :: a = [1, 2, 3, 4, 5], b

   write (*, *) a
   b = 10*a
   write (*, *) b
end program mult_array_by_scala
