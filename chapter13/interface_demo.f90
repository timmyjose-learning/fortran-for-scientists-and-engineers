! An interface is essentially equivalen to prototypes in C
! They're independent scoping units, and so to import names from the
! parent scope, use explicit `import`S

program interface_demo
   implicit none

   interface
      subroutine sort (a, n)
         implicit none

         integer, intent(in) :: n
         real, dimension(n), intent(inout) :: a
      end subroutine sort
   end interface

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, parameter :: ARR_SIZE = 10
      real, dimension(ARR_SIZE) :: array = [1.0, -2.0, 11.3, 0.0, 1.0, 6.8, 88.999, -192.99, 0.0, 10.0]

      write (*, *) 'Before sorting...'
      call print_array(a=array, n=ARR_SIZE)

      call sort(a=array, n=ARR_SIZE)

      write (*, *) 'After sorting...'
      call print_array(array, ARR_SIZE)
   end subroutine run_app

   subroutine print_array(a, n)
      implicit none

      integer, intent(in) :: n
      real, dimension(n), intent(in) :: a
      integer :: idx

      write (*, *) (a(idx), idx = 1, n)
   end subroutine
end program interface_demo

subroutine sort(a, n)
   implicit none

   integer, intent(in) :: n
   real, dimension(n), intent(inout) :: a
   integer :: i, j, max_idx
   real :: temp

   do i = 1, n - 1
      max_idx = i
      do j = i + 1, n
         if (a(j) < a(max_idx)) then
            max_idx = j
         end if
      end do

      if (max_idx /= i) then
         temp = a(max_idx)
         a(max_idx) = a(i)
         a(i) = temp
      end if
   end do
end subroutine sort