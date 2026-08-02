module types_m
   use, intrinsic :: iso_fortran_env, only: real32, real64
   implicit none
   private

   integer, parameter :: SGL = real32
   integer, parameter :: DBL = real64

   public :: SGL, DBL
end module types_m

module utils_m
   use types_m, only: SGL, DBL
   implicit none
   private

   interface findmax
      module procedure findmax_i
      module procedure findmax_s
      module procedure findmax_d
      module procedure findmax_cs
      module procedure findmax_cd
   end interface findmax

   public :: findmax

contains
   subroutine findmax_i(arr, maxval, pos_maxval)
      implicit none

      integer, dimension(:), intent(in) :: arr
      integer, intent(out) :: maxval
      integer, intent(out), optional :: pos_maxval

      integer :: pos = 0
      integer :: mx
      integer :: i

      if (size(arr) < 1) then
         error stop 'empty array'
      end if

      mx = arr(1)
      pos = 1

      do  i = 2, size(arr)
         if (arr(i) > arr(pos)) then
            mx = arr(i)
            pos = i
         end if
      end do

      maxval = mx
      if (present(pos_maxval)) then
         pos_maxval = pos
      end if
   end subroutine findmax_i

   subroutine findmax_s(arr, maxval, pos_maxval)
      implicit none

      real(kind=SGL), dimension(:), intent(in) :: arr
      real(kind=SGL), intent(out) :: maxval
      integer, intent(out), optional :: pos_maxval

      integer :: pos = 0
      real(kind=SGL) :: mx
      integer :: i

      if (size(arr) < 1) then
         error stop 'empty array'
      end if

      mx = arr(1)
      pos = 1

      do  i = 2, size(arr)
         if (arr(i) > arr(pos)) then
            mx = arr(i)
            pos = i
         end if
      end do

      maxval = mx
      if (present(pos_maxval)) then
         pos_maxval = pos
      end if
   end subroutine findmax_s

   subroutine findmax_d(arr, maxval, pos_maxval)
      implicit none

      real(kind=DBL), dimension(:), intent(in) :: arr
      real(kind=DBL), intent(out) :: maxval
      integer, intent(out), optional :: pos_maxval

      integer :: pos = 0
      real(kind=DBL) :: mx
      integer :: i

      if (size(arr) < 1) then
         error stop 'empty array'
      end if

      mx = arr(1)
      pos = 1

      do  i = 2, size(arr)
         if (arr(i) > arr(pos)) then
            mx = arr(i)
            pos = i
         end if
      end do

      maxval = mx
      if (present(pos_maxval)) then
         pos_maxval = pos
      end if
   end subroutine findmax_d

   subroutine findmax_cs(arr, maxval, pos_maxval)
      implicit none

      complex(kind=SGL), dimension(:), intent(in) :: arr
      complex(kind=SGL), intent(out) :: maxval
      integer, intent(out), optional :: pos_maxval

      integer :: pos = 0
      complex(kind=SGL) :: mx
      integer :: i

      if (size(arr) < 1) then
         error stop 'empty array'
      end if

      mx = arr(1)
      pos = 1

      do  i = 2, size(arr)
         if (abs(arr(i)) > abs(arr(pos))) then
            mx = arr(i)
            pos = i
         end if
      end do

      maxval = mx
      if (present(pos_maxval)) then
         pos_maxval = pos
      end if
   end subroutine findmax_cs

   subroutine findmax_cd(arr, maxval, pos_maxval)
      implicit none

      complex(kind=DBL), dimension(:), intent(in) :: arr
      complex(kind=DBL), intent(out) :: maxval
      integer, intent(out), optional :: pos_maxval

      integer :: pos = 0
      complex(kind=DBL) :: mx
      integer :: i

      if (size(arr) < 1) then
         error stop 'empty array'
      end if

      mx = arr(1)
      pos = 1

      do  i = 2, size(arr)
         if (abs(arr(i)) > abs(arr(pos))) then
            mx = arr(i)
            pos = i
         end if
      end do

      maxval = mx
      if (present(pos_maxval)) then
         pos_maxval = pos
      end if
   end subroutine findmax_cd

end module utils_m

program generic_maxval_single_module
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   use types_m, only: SGL, DBL
   use utils_m, only: findmax
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, parameter :: ARR_SIZE = 10
      integer, dimension(ARR_SIZE) :: i_arr = [1, 2, 1, 0, 3, 5, -99, 99, 11, 98]
      real(kind=SGL), dimension(ARR_SIZE) :: sgl_arr = [1.1, -1.1, 0.0, 12.233, -12.23233, 10202020.11, -1191991.99, 122.199, &
         190.1022, 19.99]
      real(kind=DBL), dimension(ARR_SIZE) :: dbl_arr = [1.1, -1.1, 0.0, 12.233, -12.23233, 10202020.11, -1191991.99, 122.199, &
         190.1022, 19.99]
      complex(kind=SGL), dimension(ARR_SIZE) :: sgl_cmplx_arr = [(1.0, 2.0), (-2.0, 1.0), (11.2992, -18928.22), (11.298282, &
         11.29292),(11.23, -3737.3), (0.002, -0.2323), (8912.33, 882.22), (0.0, 0.0), (0.00, 99.1991), (11.20, 9383.888) ]
      complex(kind=DBL), dimension(ARR_SIZE) :: dbl_cmplx_arr = [(1.0, 2.0), (-2.0, 1.0), (11.2992, -18928.22), (11.298282, 11.29292), &
         (11.23, -3737.3), (0.002, -0.2323), (8912.33, 882.22), (0.0, 0.0), (0.00, 99.1991), (11.20, 9383.888)]

      integer :: ival
      real(kind=SGL) :: sval
      real(kind=DBL) :: dval
      complex(kind=SGL) :: c_sval
      complex(kind=DBL) :: c_dval
      integer :: pos_maxval

      call findmax(i_arr, ival)
      write (output_unit, *) 'Max value = ', ival

      call findmax(i_arr, ival, pos_maxval=pos_maxval)
      write (output_unit, *) 'Max value = ', ival, ', and position = ', pos_maxval

      call findmax(i_arr, ival, pos_maxval)
      write (output_unit, *) 'Max value = ', ival, ', and position = ', pos_maxval

      call findmax(sgl_arr, sval, pos_maxval)
      write (output_unit, *) 'Max value = ', sval, ', and position = ', pos_maxval

      call findmax(dbl_arr, dval, pos_maxval)
      write (output_unit, *) 'Max value = ', dval, ', and position = ', pos_maxval

      call findmax(sgl_cmplx_arr, c_sval, pos_maxval)
      write (output_unit, *) 'Max value = ', c_sval, ', and position = ', pos_maxval

      call findmax(dbl_cmplx_arr, c_dval, pos_maxval)
      write (output_unit, *) 'Max value = ', c_dval, ', and position = ', pos_maxval
   end subroutine run_app
end program generic_maxval_single_module