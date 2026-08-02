module sort_m
   implicit none
   private

   ! generic interface
   interface sort
      module procedure sorti
      module procedure sortr
      module procedure sortc
   end interface sort

   public :: sort

contains
   ! implementations of the generic interface
   subroutine sorti(arr, nvals)
      implicit none
      integer, intent(in) :: nvals
      integer, dimension(nvals), intent(inout) :: arr

      integer :: i, j, max_idx
      integer :: temp

      do i = 1, nvals - 1
         max_idx = i
         do j = i + 1, nvals
            if (arr(j) < arr(max_idx)) then
               max_idx = j
            end if
         end do

         if (max_idx /= i) then
            temp = arr(max_idx)
            arr(max_idx) = arr(i)
            arr(i) = temp
         end if
      end do
   end subroutine sorti

   subroutine sortr(arr, nvals)
      implicit none
      integer, intent(in) :: nvals
      real, dimension(nvals), intent(inout) :: arr

      integer :: i, j, max_idx
      real :: temp

      do i = 1, nvals - 1
         max_idx = i
         do j = i + 1, nvals
            if (arr(j) < arr(max_idx)) then
               max_idx = j
            end if
         end do

         if (max_idx /= i) then
            temp = arr(max_idx)
            arr(max_idx) = arr(i)
            arr(i) = temp
         end if
      end do
   end subroutine sortr

   subroutine sortc(arr, nvals)
      implicit none
      integer, intent(in) :: nvals
      character, dimension(nvals), intent(inout) :: arr

      integer :: i, j, max_idx
      character :: temp

      do i = 1, nvals - 1
         max_idx = i
         do j = i + 1, nvals
            if (lle(arr(j), arr(max_idx))) then
               max_idx = j
            end if
         end do

         if (max_idx /= i) then
            temp = arr(max_idx)
            arr(max_idx) = arr(i)
            arr(i) = temp
         end if
      end do
   end subroutine sortc
end module sort_m


program generic_sort_single_module
   use, intrinsic :: iso_fortran_env, only: output_unit
   use sort_m, only: sort
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, dimension(5) :: iarr = [1, 2, 0, 1, 5]
      real, dimension(5) :: rarr = [-2.1, 0.0, -2.1, 2.5, 5.5]
      character, dimension(5) :: carr = ['h', 'e', 'l', 'l', 'o']

      call sort(iarr, 5)
      write (output_unit, *) iarr

      call sort(rarr, 5)
      write (output_unit, *) rarr

      call sort(carr, 5)
      write (output_unit, *) carr
   end subroutine run_app
end program generic_sort_single_module