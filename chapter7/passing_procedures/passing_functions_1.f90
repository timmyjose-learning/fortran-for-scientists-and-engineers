program passing_functions_1
   implicit none

   integer :: num
   integer :: iostat
   character(len=512) :: iomsg

   interface
      integer function square(x)
         implicit none

         integer, intent(in) :: x
      end function square

      integer function cube(x)
         implicit none

         integer, intent(in) :: x
      end function cube
   end interface

   write (*, *) 'Enter an integer'
   read (*, *, iostat=iostat, iomsg=iomsg) num

   if (iostat /= 0) then
      write (*, *) 'Error: ', iomsg
   else
      call call_fun(square, num)
      call call_fun(cube, num)
   end if
contains
   subroutine call_fun(fun, x)
      implicit none

      ! `external` is largely not needed in Modern Fortran
      integer, external :: fun
      integer, intent(in) :: x

      write (*, *) fun(x)
   end subroutine call_fun
end program passing_functions_1

integer function square(x)
   implicit none

   integer, intent(in) :: x

   square = x**2
end function square

integer function cube(x)
   implicit none

   integer, intent(in) :: x

   cube = x*x*x
end function cube
