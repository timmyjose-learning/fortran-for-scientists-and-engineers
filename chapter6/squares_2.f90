program squares_2
   implicit none

   integer :: i
   integer, dimension(-5:5) :: nums, squares

   do i = -5, 5
      nums(i) = i
      squares(i) = i**2
   end do

   do i = -5, 5
      write (*, '(A, I6, A, I6)') 'Number = ', nums(i), ', Square = ', squares(i)
   end do
end program squares_2
