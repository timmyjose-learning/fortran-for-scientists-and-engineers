program select_type_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   type :: point_t
      real :: x
      real :: y
   end type point_t

   type, extends(point_t) :: point3d_t
      real :: z
   end type point3d_t

   type, extends(point_t) :: point_temp
      real :: temp
   end type point_temp

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(point_t), target :: p2
      type(point3d_t), target :: p3
      type(point_temp), target :: ptemp
      class(point_t), pointer :: p

      p2%x = 1.0
      p2%y = 2.0

      p3%x = -1.0
      p3%y = -2.0
      p3%z = -3.0

      ptemp%x = 3.12
      ptemp%y = -99.22
      ptemp%temp = 123.45

      p => p2
      call dispatch(p)

      p => p3
      call dispatch(p)

      p => ptemp
      call dispatch(p)
   end subroutine run_app

   subroutine dispatch(point)
      implicit none

      class(point_t), pointer, intent(in) :: point

      select type (point)
       type is (point_temp)
         write (output_unit, '("Deteted point_temp: x = ",F8.3,", y = ",F8.3,", temp = ",F8.3)') point%x, point%y, point%temp
       type is (point3d_t)
         write (output_unit, '("Detected point3d: x = ",F8.3,", y = ",F8.3, ", z = ",F8.3)') point%x, point%y, point%z
       class is (point_t)
         write (output_unit, '("Class is point_t, x = ",F8.3,", y = ",F8.3)') point%x, point%y
       class default
         write (output_unit, *) 'Some other type or class'
      end select
   end subroutine dispatch
end program select_type_demo