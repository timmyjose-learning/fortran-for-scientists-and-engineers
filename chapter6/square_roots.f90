program square_roots
   implicit none

   integer :: i
   integer, dimension(10) :: a = [(i, i=1, 10)]
   real, dimension(10) :: roots

   do i = 1, 10
      roots(i) = sqrt(real(a(i)))
   end do

   do i = 1, 10
      write (*, '(A, I6, A, F8.3)') 'Number = ', a(i), ', Square Root = ', roots(i)
   end do
end program square_roots
