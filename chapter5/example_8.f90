program example_8
   implicit none

   character(len=10) :: first_name = 'James     '
   character :: initial = 'R'
   character(len=16) :: last_name = 'Johnson         '
   integer :: grade = 92
   character(len=9) :: class = 'COSC 2301'

   write (*, '(A10, 1X, A1, 1X, A10, 4X, I3, T51, A9)') first_name, initial, last_name, grade, class
end program example_8
