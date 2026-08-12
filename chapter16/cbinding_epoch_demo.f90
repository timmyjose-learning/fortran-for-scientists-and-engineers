program cbinding_epoch_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_null_ptr
   implicit none

   interface
      function get_ctime(timer) result(time) bind(c, name='time')
         import :: c_int, c_ptr

         type(c_ptr), value :: timer
         integer(kind=c_int) :: time
      end function get_ctime

      function csleep(seconds) result(unslept_seconds) bind(c, name='sleep')
         import :: c_int

         integer(kind=c_int), value :: seconds
         integer(kind=c_int) :: unslept_seconds
      end function csleep
   end interface

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer(kind=c_int) :: epoch
      integer(kind=c_int) :: res
      integer :: i

      do i = 1, 10
         epoch = get_ctime(c_null_ptr)
         write (output_unit, *) epoch
         res = csleep(1_c_int)
      end do
   end subroutine run_app
end program cbinding_epoch_demo