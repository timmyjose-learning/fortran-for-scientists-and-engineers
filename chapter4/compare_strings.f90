program compare_strings
   implicit none

   character(len=20) :: s1, s2
   integer :: idx
   character(len=20) :: s1_normalised, s2_normalised

   write (*, *) 'Enter the first string'
   read (*, *) s1

   write (*, *) 'Enter the second string'
   read (*, *) s2

   s1_normalised = s1 ! copy
   s2_normalised = s2

   do idx = 1, len(s1)
   if (s1(idx:idx) >= 'a' .and. s1(idx:idx) <= 'z') then
      s1_normalised(idx:idx) = achar(iachar(s1(idx:idx)) - 32)
   end if
   end do

   do idx = 1, len(s2)
   if (s2(idx:idx) >= 'a' .and. s2(idx:idx) <= 'z') then
      s2_normalised(idx:idx) = achar(iachar(s2(idx:idx)) - 32)
   end if
   end do

   if (s1_normalised == s2_normalised) then
      write (*, *) 'The strings are equal'
   else
      write (*, *) 'The strings are not equal'
   end if
end program compare_strings
