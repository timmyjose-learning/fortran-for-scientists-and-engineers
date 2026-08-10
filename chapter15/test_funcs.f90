module unary_functions_m
   implicit none
   private

   public :: func1, func2, func3

contains
   function func1(x) result(res)
      implicit none

      real, intent(in) :: x
      real :: res

      res = x**2 - 2*x + 4
   end function func1

   function func2(x) result(res)
      implicit none

      real, intent(in) :: x
      real :: res

      res = exp(-x/5) ** sin(2*x)
   end function func2

   function func3(x) result(res)
      implicit none

      real, intent(in) :: x
      real :: res

      res = cos(x)
   end function func3
end module unary_functions_m

program test_funcs
   use, intrinsic :: iso_fortran_env, only: input_unit,  output_unit, error_unit
   use unary_functions_m, only: func1, func2, func3
   implicit none

   interface
      function unary_real_fn(x) result(res)
         implicit none

         real, intent(in) :: x
         real :: res
      end function unary_real_fn
   end interface

   call run_app

contains
   subroutine run_app()
      implicit none

      real :: x
      procedure(unary_real_fn), pointer :: ufunc
      integer :: choice

      write (output_unit, *) 'Enter a real number'
      read (input_unit, *) x

      write (output_unit, *) 'Enter your choice: 1 func1, 2 func2, 3 func3'
      read (input_unit, *) choice

      select case(choice)
       case (1)
         ufunc => func1
       case (2)
         ufunc => func2
       case (3)
         ufunc => func3
       case default
         return
      end select

      write (output_unit, *) ufunc(x)
   end subroutine run_app
end program test_funcs
