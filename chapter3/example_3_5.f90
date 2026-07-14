program example_3_5
   implicit none

   integer :: day_of_week

   write (*, *) 'Enter the day of the week (1-7)'
   read (*, *) day_of_week

   select case (day_of_week)
   case (1)
      write (*, *) 'Sunday'
   case (2)
      write (*, *) 'Monday'
   case (3)
      write (*, *) 'Tuesday'
   case (4)
      write (*, *) 'Wednesday'
   case (5)
      write (*, *) 'Thursday'
   case (6)
      write (*, *) 'Friday'
   case (7)
      write (*, *) 'Saturday'
   case default
      write (*, *) 'Not a valid day of the week'
   end select
end program example_3_5
