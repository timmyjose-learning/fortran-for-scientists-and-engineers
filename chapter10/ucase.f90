program ucase
   implicit none

   character(len=512) :: s1 = 'Hello, world'

   write (*, *) trim(s1)
   call to_upper(s1)
   write (*, *) trim(s1)

contains
   subroutine to_upper(str)
      implicit none

      integer, parameter :: CONV_DIFF = 32
      character(len=*), intent(inout) :: str
      integer :: i

      do i = 1, len_trim(str)
         if (lge(str(i:i), 'a') .and. lle(str(i:i), 'z')) then ! string indexing is done via (lbound:ubound)
            str(i:i) = achar(iachar(str(i:i)) - CONV_DIFF)
         end if
      end do
   end subroutine to_upper
end program ucase