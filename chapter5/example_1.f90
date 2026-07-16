program example_1
   implicit none

   integer :: i = 21
   real :: r = 3.141593

   write (*, 100) i, r
100 format('The result for iteration ', I3, ' is ', F7.3)
   write (*, '(A, I3, A, F7.3)') 'The result for iteration ', i, ' is ', r
end program example_1
