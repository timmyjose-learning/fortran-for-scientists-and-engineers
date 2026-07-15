program doy
   implicit none

   integer :: day, month, year
   integer :: leap_day = 0, idx = 0
   integer :: day_of_year

   write (*, *) 'Enter the day, month, and year'
   read (*, *) day, month, year

   if (mod(year, 400) == 0) then
      leap_day = 1
   else if (mod(year, 100) == 0) then
      leap_day = 0
   else if (mod(year, 4) == 0) then
      leap_day = 1
   else
      leap_day = 0
   end if

   day_of_year = day
   do idx = 1, month - 1
      select case (idx)
      case (1, 3, 5, 7, 8, 10, 12)
         day_of_year = day_of_year + 31
      case (4, 6, 9, 11)
         day_of_year = day_of_year + 30
      case (2)
         day_of_year = day_of_year + 28 + leap_day
      end select
   end do

   write (*, *) day_of_year
end program doy
