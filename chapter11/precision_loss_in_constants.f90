program precision_loss_in_constants
   implicit none

   integer, parameter :: DBL = selected_real_kind(p=13)
   real(kind=DBL) :: r1 = 6.6666666666666 ! loses precision
   real(kind=DBL) :: r2 = 6.6666666666666_DBL ! maintains precision because of the explicit suffix

   write (*, *) 'r1 = ', r1
   write (*, *) 'r2 = ', r2
end program precision_loss_in_constants