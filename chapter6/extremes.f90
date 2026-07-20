program extremes
   implicit none

   integer, parameter :: MAX_SIZE = 10
   integer :: i
   integer :: nvals, largest, smallest
   integer, dimension(MAX_SIZE) :: a

   write (*, '(A,I6, A)') 'How many value (1 - ', MAX_SIZE, ') ?'
   read (*, *) nvals

   if (nvals > MAX_SIZE) then
      write (*, *) 'Too many values'
      error stop
   else if (nvals < 1) then
      write (*, *) 'Too few values'
      error stop
   end if

   do i = 1, nvals
      write (*, *) 'Enter value'
      read (*, *) a(i)
   end do

   largest = a(1)
   smallest = a(1)
   do i = 2, nvals
      if (a(i) > largest) then
         largest = a(i)
      end if

      if (a(i) < smallest) then
         smallest = a(i)
      end if
   end do

   write (*, '(A, I6, A, I6)') 'Smallest = ', smallest, ', largest = ', largest
end program extremes
