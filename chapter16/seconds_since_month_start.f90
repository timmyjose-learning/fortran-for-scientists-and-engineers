program seconds_since_month_start
   use, intrinsic :: iso_fortran_env, only: output_unit, real64

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, dimension(8) :: values
      real(kind=real64) :: seconds

      call date_and_time(values=values)
      seconds = 86400.00_real64 * values(3) + 3600.00_real64 * values(5) + 60.00_real64 + values(6) + &
         values(7) + 0.001_real64 * values(8)
      write (output_unit, *) seconds
   end subroutine run_app
end program seconds_since_month_start

