program factorial_subroutine
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   implicit none

   integer :: num
   integer :: result

   write (output_unit, *) 'Enter the number'
   read (input_unit, *) num

   call factorial(num, result)
   write (output_unit, * ) 'The factorial of ', num, ' is ', result

contains
   recursive subroutine factorial(n, res)
      implicit none

      integer, intent(in) :: n
      integer, intent(inout) :: res
      integer :: temp

      if (n <= 1) then
         res = 1
      else
         call factorial (n - 1, temp)
         res = n * temp
      end if
   end subroutine factorial
end program factorial_subroutine