program test_char1
   implicit none

   character(len=8) :: a, b, c

   a = 'ABCDEFGHIJ'
   b = '12345678'
   c = a(5:7)
   b(7:8) = a(2:6)

   write (*, *) a
   write (*, *) b
   write (*, *) c
end program test_char1
