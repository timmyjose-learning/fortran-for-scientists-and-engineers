program square_and_cube_roots
   implicit none

   integer :: i
   real, dimension(10) :: nums, square_roots, cube_roots

   do i = 1, 10
      nums(i) = real(i)
      square_roots(i) = sqrt(real(i))
      cube_roots(i) = i**(1.0/3.0)
   end do

   write (*, '(A,2X,A,2X,A)') 'Number', 'Square Root', 'Cube Root'
   write (*, '(3(F8.5))') (nums(i), square_roots(i), cube_roots(i), i=1, 10)
end program square_and_cube_roots
