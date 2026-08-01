module module_example
   implicit none
   private

   real :: x = 100.0
   real :: y = 200.0

   public :: x, y
end module module_example

program scope_test
   use module_example
   implicit none

   integer :: i = 1, j = 2

   write (*, *) 'Beginning: ', i, j, x, y
   call sub1(i, j)
   write (*, *) 'After sub1: ', i, j, x, y
   call sub2
   write (*, *) 'After sub2: ', i, j, x, y

contains
   subroutine sub2()
      implicit none

      real :: x
      x = 1000.0
      y = 2000.0

      write (*, *) 'In sub2: ', x, y
   end subroutine sub2
end program scope_test

subroutine sub1 (i, j)
   implicit none

   integer, intent(inout) :: i, j
   integer, dimension(5) :: array

   write (*, *) 'In sub1 before sub2: ', i, j
   call sub2()
   write (*, *) 'In sub1 after sub2: ', i, j

   array = [(1000 * i, i = 1, 5)]
   write (*, *) 'After array definition in sub2: ', i, j, array

contains
   subroutine sub2
      integer :: i

      i = 1000
      j = 2000
      write (*, *) 'In sub1 in sub2: ', i, j
   end subroutine sub2
end subroutine sub1
