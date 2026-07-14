program even_odd
   implicit none

   integer :: num

   write (*, *) 'Enter a number between 1 and 10'
   read (*, *) num

   select case (num)
   case (1, 3, 5, 7, 9)
      write (*, *) 'Odd'
   case (2, 4, 6, 8, 10)
      write (*, *) 'Even'
   case default
      write (*, *) 'You didn''t enter a number in the expected range'
   end select
end program even_odd
