program passing_subroutines_1
   implicit none

   integer :: num
   integer :: result
   integer :: iostat
   character(len=512) :: iomsg

   interface
      subroutine square(x, res)
         implicit none

         integer, intent(in) :: x
         integer, intent(out) :: res
      end subroutine square

      subroutine cube(x, res)
         implicit none

         integer, intent(in) :: x
         integer, intent(out) :: res
      end subroutine cube
   end interface

   write (*, *) 'Enter an integer'
   read (*, *, iostat=iostat, iomsg=iomsg) num

   if (iostat /= 0) then
      write (*, *) 'Error: ', iomsg
   else
      call call_sub(square, num, result)
      write (*, *) result

      call call_sub(cube, num, result)
      write (*, *) result
   end if

contains
   subroutine call_sub(sub, x, res)
      implicit none

      interface
         subroutine sub(val, ret)
            implicit none

            integer, intent(in) :: val
            integer, intent(out) :: ret
         end subroutine sub
      end interface

      integer, intent(in) :: x
      integer, intent(out) :: res

      call sub(x, res)
   end subroutine call_sub
end program passing_subroutines_1

subroutine square(x, res)
   implicit none

   integer, intent(in) :: x
   integer, intent(out) :: res

   res = x**2
end subroutine square

subroutine cube(x, res)
   implicit none

   integer, intent(in) :: x
   integer, intent(out) :: res

   res = x*x*x
end subroutine cube

