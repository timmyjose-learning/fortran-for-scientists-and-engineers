program squares
   implicit none

   integer, dimension(10) :: number, square
   integer :: idx

   do idx = 1, 10
      number(idx) = idx
      square(idx) = idx**2
   end do

   do idx = 1, 10
      write (*, '(A, I6, A, I6)') 'Number = ', number(idx), ', Square = ', square(idx)
   end do
end program squares
