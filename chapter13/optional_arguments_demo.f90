module procs_m
   implicit none
   private

   public :: calc_extremes

contains
   subroutine calc_extremes(arr, minval, pos_minval, maxval, pos_maxval)
      implicit none

      real, dimension(:), intent(in) :: arr
      real, intent(out), optional :: minval
      real, intent(out), optional :: maxval
      integer, intent(out), optional :: pos_minval
      integer, intent(out), optional :: pos_maxval

      integer :: idx
      real :: minv, maxv
      integer :: pos_minv, pos_maxv

      if ( size(arr) < 1) then
         error stop 'Invalid array'
      end if

      minv = arr(1)
      pos_minv = 1
      maxv = arr(1)
      pos_maxv = 1

      do idx = 2, size(arr)
         if (arr(idx) < minv) then
            minv = arr(idx)
            pos_minv = idx
         end if

         if (arr(idx) > maxv) then
            maxv = arr(idx)
            pos_maxv = idx
         end if
      end do

      if (present(minval)) then
         minval = minv
      end if

      if (present(pos_minval)) then
         pos_minval = pos_minv
      end if

      if (present(maxval)) then
         maxval = maxv
      end if

      if (present(pos_maxval)) then
         pos_maxval = pos_maxv
      end if
   end subroutine calc_extremes
end module procs_m

program optional_arguments_demo
   use procs_m, only: calc_extremes
   implicit none

   real, dimension(10) :: arr = [1.0, -2.0, 0.0, 1.0, 2.0, 3.0, 4.0, 11.0, -99.0, 0.0]
   real :: maxval, minval
   integer :: pos_minval, pos_maxval

   ! Normal call
   call calc_extremes(arr, minval, pos_minval, maxval, pos_maxval)
   write (*, *) minval, pos_minval, maxval, pos_maxval

   ! Call with keyword arguments
   call calc_extremes(arr, minval=minval, pos_minval=pos_minval)
   write (*, *) minval, pos_minval

   call calc_extremes(arr, maxval=maxval, pos_maxval=pos_maxval)
   write (*, *) maxval, pos_maxval

   call calc_extremes(arr, maxval=maxval, pos_minval=pos_minval)
   write (*, *) maxval, pos_minval
end program optional_arguments_demo