program weekday_weekend
   implicit none

   character(len=11) :: day_of_week
   character(len=11) :: day_type

   write (*, *) 'Enter the day of the week (Sunday, etc.)'
   read (*, *) day_of_week

   select case (day_of_week)
   case ('Saturday', 'Sunday')
      day_type = 'Weekend'
   case ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
      day_type = 'Weekday'
   case default
      day_type = 'Invalid day'
   end select

   write (*, *) day_type
end program weekday_weekend
