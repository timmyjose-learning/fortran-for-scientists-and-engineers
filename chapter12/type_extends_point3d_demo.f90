module point_m
   use, intrinsic :: iso_fortran_env, only: real64

   integer, parameter :: REAL_K = real64

   type :: point_2d_t
      real(kind=REAL_K) :: x
      real(kind=REAL_K) :: y

   contains
      procedure :: write_formatted => write_point_2d_t
      generic :: write(formatted) => write_formatted
   end type point_2d_t

   type, extends(point_2d_t) :: point_3d_t
      real(kind=REAL_K) :: z

   contains
      procedure :: write_formatted => write_point_3d_t
   end type point_3d_t

   public :: REAL_K, point_2d_t, point_3d_t, add_2d_points, sub_2d_points, add_3d_points, sub_3d_points

contains
   function add_2d_points(p1, p2) result(sum)
      implicit none

      type(point_2d_t), intent(in) :: p1, p2
      type(point_2d_t) :: sum

      sum%x = p1%x + p2%x
      sum%y = p1%y + p2%y
   end function add_2d_points

   function sub_2d_points(p1, p2) result(diff)
      implicit none

      type(point_2d_t), intent(in) :: p1, p2
      type(point_2d_t) :: diff

      diff%x = p1%x - p2%x
      diff%y = p1%y - p2%y
   end function sub_2d_points

   function add_3d_points(p1, p2) result(sum)
      implicit none

      type(point_3d_t), intent(in) :: p1, p2
      type(point_3d_t) :: sum

      sum%x = p1%x + p2%x
      sum%y = p1%y + p2%y
      sum%z = p1%z + p2%z
   end function add_3d_points

   function sub_3d_points(p1, p2) result(diff)
      implicit none

      type(point_3d_t), intent(in) :: p1, p2
      type(point_3d_t) :: diff

      diff%x = p1%x - p2%x
      diff%y = p1%y - p2%y
      diff%z = p1%z - p2%z
   end function sub_3d_points

   subroutine write_point_2d_t(pt, unit, iotype, vlist, iostat, iomsg)
      implicit none

      class(point_2d_t), intent(in) :: pt
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, dimension(:), intent(in) :: vlist
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
      character(len=24) :: xstr, ystr

      if (.false.) then
         if (len(iotype) == 0) return
         if (size(vlist) == 0) return
      end if

      write (xstr, '(F8.3)') pt%x
      write (ystr, '(F8.3)') pt%y

      write (unit, '("(",A,", ",A,")")', iostat=iostat, iomsg=iomsg) trim(adjustl(xstr)), trim(adjustl(ystr))
   end subroutine write_point_2d_t

   subroutine write_point_3d_t(pt, unit, iotype, vlist, iostat, iomsg)
      implicit none
      class(point_3d_t), intent(in) :: pt
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, dimension(:), intent(in) :: vlist
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
      character(len=24) :: xstr, ystr, zstr

      if (.false.) then
         if (len(iotype) == 0) return
         if (size(vlist) == 0) return
      end if

      write (xstr, '(F8.3)') pt%x
      write (ystr, '(F8.3)') pt%y
      write (zstr, '(F8.3)') pt%z

      write (unit, '("(",A,", ",A,", ",A,")")', iostat=iostat, iomsg=iomsg) trim(adjustl(xstr)), trim(adjustl(ystr)), &
         trim(adjustl(zstr))
   end subroutine write_point_3d_t
end module point_m

program type_extends_point3d_demo
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   use point_m, only: point_2d_t, point_3d_t, add_2d_points, sub_2d_points, add_3d_points, sub_3d_points

   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(point_2d_t) :: p1, v2
      type(point_3d_t) :: pp1, pp2

      write (output_unit, *) 'Enter the first 2d point'
      read (input_unit, *) p1%x, p1%y

      write (output_unit, *) 'Enter the second 2d point'
      read (input_unit, *) v2%x, v2%y

      write (output_unit, *) 'Sum = ', add_2d_points(p1, v2)
      write (output_unit, *) 'Difference = ', sub_2d_points(p1, v2)

      write (output_unit, '(/)')

      write (output_unit, *) 'Enter the first 3d point'
      read (input_unit, *) pp1%x, pp1%y, pp1%z

      write (output_unit, *) 'Enter the second 3d point'
      read (input_unit, *) pp2%x, pp2%y, pp2%z

      write (output_unit, *) 'Sum = ', add_3d_points(pp1, pp2)
      write (output_unit, *) 'Difference = ', sub_3d_points(pp1, pp2)
   end subroutine run_app
end program type_extends_point3d_demo