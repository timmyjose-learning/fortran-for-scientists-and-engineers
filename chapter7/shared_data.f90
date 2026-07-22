! Compiler to a `mod` file using:
! gfortran -c shared_data.f90
module shared_data
   implicit none

   ! Persist the data between `use` invocations.
   ! In Modern Fortran, this is done automatically
   save

   integer, parameter :: NUM_VALS = 5
   real, dimension(NUM_VALS) :: values
end module shared_data
