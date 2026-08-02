module sort_m
   implicit none
   private

   interface
      module subroutine sorti(arr, nvals)
         implicit none
         integer, intent(in) :: nvals
         integer, dimension(nvals), intent(inout) :: arr
      end subroutine sorti

      module subroutine sortr(arr, nvals)
         implicit none
         integer, intent(in) :: nvals
         real, dimension(nvals), intent(inout) :: arr
      end subroutine sortr

      module subroutine sortc(arr, nvals)
         implicit none
         integer, intent(in) :: nvals
         character, dimension(nvals), intent(inout) :: arr
      end subroutine sortc
   end interface

   ! generic interface
   interface sort
      module procedure sorti
      module procedure sortr
      module procedure sortc
   end interface sort

   public :: sort
end module sort_m

submodule (sort_m) sort_impl_m
   implicit none

contains
   module subroutine sorti(arr, nvals)
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

   module subroutine sortr(arr, nvals)
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

   module subroutine sortc(arr, nvals)
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
end submodule sort_impl_m

program generic_sort_multiple_modules
   use, intrinsic :: iso_fortran_env, only: output_unit
   use sort_m, only: sort
   implicit none

   call run_app()

contains

   subroutine run_app()
      implicit none

      integer, parameter :: ARR_SIZE = 5
      integer, dimension(ARR_SIZE) :: iarr = [1, 2, 0, 5, 1]
      real, dimension(ARR_SIZE) :: rarr = [-0.5, 0.0, -2.5, 1.1, 11.99]
      character, dimension(ARR_SIZE) :: carr = ['h', 'e', 'l', 'l', 'o']

      call sort(iarr, ARR_SIZE)
      write (output_unit, *) iarr

      call sort(rarr, ARR_SIZE)
      write (output_unit, *) rarr

      call sort(carr, ARR_SIZE)
      write (output_unit, *) carr
   end subroutine run_app
end program generic_sort_multiple_modules