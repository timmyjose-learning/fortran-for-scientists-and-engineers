program basic_error_handling
   implicit none

   real :: x, y, result
   integer :: proc_err

   write (*, *) 'Enter two real numbers'
   read (*, *) x, y

   call process(x, y, result, proc_err)

   if (proc_err /= 0) then
      write (*, *) 'Error: cannot find sqrt of a negative value'
   else
      write (*, *) 'result = ', result
   end if

contains
   subroutine process(a, b, res, err)
      implicit none

      real, intent(in) :: a, b
      real, intent(out) :: res
      integer, intent(out) :: err
      real :: diff

      diff = a - b

      if (diff < 0.0) then
         res = 0.0
         err = 1
      else
         res = sqrt(diff)
         err = 0
      end if
   end subroutine process
end program basic_error_handling
