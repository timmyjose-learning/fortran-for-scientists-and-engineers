! rAw
program example_7
   implicit none

   character(len=17) :: string = 'This is a string'

   write (*, '(A)') string
   write (*, '(A20)') string
   write (*, '(A6)') string
end program example_7
