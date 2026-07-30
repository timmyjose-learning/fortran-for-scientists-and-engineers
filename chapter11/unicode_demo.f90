program unicode_demo
   implicit none

   call demo()

contains
   subroutine demo()
      implicit none
      integer, parameter :: UNICODE_K = selected_char_kind('ISO_10646')
      character(kind=UNICODE_K, len=:), allocatable :: str

      if (UNICODE_K == -1) then
         error stop 'Unicode not supported'
      end if

      allocate(character(kind=UNICODE_K, len=1024) :: str)

      write (*, *) 'Enter any string you like'
      read (*, '(A)') str

      write (*, *) 'You entered "', trim(str), '"'
   end subroutine demo
end program unicode_demo