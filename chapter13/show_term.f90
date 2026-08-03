program show_term
   implicit none

   character(len=64) :: term_val
   integer :: status

   call get_environment_variable("TERM", value=term_val, status=status, trim_name=.true.)
   if (status /= 0) then
      error stop 'Failed to get env var'
   end if

   write (*, *) term_val
end program show_term