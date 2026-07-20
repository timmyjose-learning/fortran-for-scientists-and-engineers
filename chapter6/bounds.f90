program bounds
   implicit none

   integer :: i
   real, dimension(5) :: a = (/1.0, 2.0, 3.0, 4.0, 5.0/)
   real, dimension(5) :: b = (/10.0, 20.0, 30.0, 40.0, 50.0/)

   ! out of bounds - behaviour depends on whether bouns checking has been enabled or not, as well
   ! as on the compiler
   do i = 1, 6
      write (*, *) a(i)
   end do
end program bounds
