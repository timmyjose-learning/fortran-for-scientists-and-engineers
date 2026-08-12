program date_and_time_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, dimension(8) :: values

      call date_and_time(values=values)

      write(output_unit, *) values
   end subroutine run_app
end program date_and_time_demo