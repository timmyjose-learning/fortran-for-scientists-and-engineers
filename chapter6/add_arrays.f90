! whole array operations can be performed if the arrays have the same shape: the same rank, and the same extent in each dimension.
program add_array
   implicit none

   integer, parameter :: ARR_SIZE = 5
   real, dimension(ARR_SIZE) :: a = [1.0, 2.0, 3.0, 4.0, 5.0], b = [10.0, 20.0, 30.0, 40.0, 50.0]
   real, dimension(ARR_SIZE) :: c, d
   integer :: i

   do i = 1, ARR_SIZE
      c(i) = a(i) + b(i)
   end do

   ! same as above
   d = a + b

   write (*, *) c
   write (*, *) d
end program add_array
