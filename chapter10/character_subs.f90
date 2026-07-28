program character_subs
   implicit none

   integer :: length

   write (*, *) 'Enter the length'
   read (*, *) length
   write (*, *) get_slice(length)

contains
   function get_slice(n) result(slice)
      implicit none

      character(len=26), parameter :: alphabet = 'abcdefghijklmnopqrstuvwxyz'
      integer, intent(in) :: n
      character(len=n) :: slice

      if (n < 0 .or. n > 26) then
         error stop 'invalid slice range' ! can leak memory
      end if

      slice = alphabet(1:n)
   end function get_slice
end program character_subs