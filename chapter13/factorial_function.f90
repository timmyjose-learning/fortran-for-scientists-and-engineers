program factorial_function
   use, intrinsic :: iso_fortran_env, only: output_unit, input_unit
   implicit none

   integer :: num

   write (output_unit, *) 'Enter the number'
   read (input_unit, *) num

   write (output_unit, *) 'The factorial of ', num, ' is ', factorial(num)

contains
   recursive function factorial(n) result(res)
      implicit none

      integer, intent(in) :: n
      integer :: res

      if (n <= 1) then
         res = 1
      else
         res = n * factorial(n - 1)
      end if
   end function factorial
end program factorial_function