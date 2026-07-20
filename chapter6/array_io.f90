program array_io
   implicit none

   real, dimension(5) :: a = [1.0, 2.0, 3.0, 4.0, 5.0]
   integer, dimension(4) :: vec = [4, 3, 4, 5]

   write (*, '(6F8.3)') a
   write (*, '(6F8.3)') a(2::2)
   write (*, '(6F8.3)') a(vec)
end program array_io
