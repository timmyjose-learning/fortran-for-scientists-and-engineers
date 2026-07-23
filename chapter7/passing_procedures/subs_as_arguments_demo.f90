program subs_as_arguments_demo
   implicit none

   external :: sum, prod
   real :: x, y, res

   write (*, *) 'Enter two real numbers'
   read (*, *) x, y

   call subs_as_arguments(x, y, sum, res)
   write (*, '(F8.3)') res

   call subs_as_arguments(x, y, prod, res)
   write (*, '(F8.3)') res
end program subs_as_arguments_demo

subroutine subs_as_arguments(x, y, sub, result)
   implicit none

   external :: sub
   real, intent(in) :: x, y
   real, intent(out) :: result

   call sub(x, y, result)
end subroutine subs_as_arguments

subroutine sum(x, y, result)
   implicit none

   real, intent(in) :: x, y
   real, intent(out) :: result

   result = x + y
end subroutine sum

subroutine prod(x, y, result)
   implicit none

   real, intent(in) :: x, y
   real, intent(out) :: result

   result = x*y
end subroutine prod
