! Compile it:
! gfortran -c test_shared_data.f90
! Then,
! Link it like so:
! gfortran shared_dat.o test_shared_data.o -o test_shared_data
program test_shared_data
   use shared_data
   implicit none

   real, parameter :: PI = 3.141593

   values = PI*[1.0, 2.0, 3.0, 4.0, 5.0]

   call sub1
end program test_shared_data

subroutine sub1
   use shared_data
   implicit none

   write (*, '(*(F8.3))') values
end subroutine sub1
