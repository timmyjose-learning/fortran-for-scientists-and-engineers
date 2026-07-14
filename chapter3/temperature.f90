program temperature
   implicit none

   integer :: temp

   write (*, *) 'What is the temperature?'
   read (*, *) temp

   select case (temp)
   case (:-1)
      write (*, *) 'It''s below freezing today!'
   case (0)
      write (*, *) 'It''s exactly at the freezing point'
   case (1:20)
      write (*, *) 'Rather pleasant weather'
   case (21:33)
      write (*, *) 'Okay, it''s warm, almost hot'
   case (34:)
      write (*, *) 'Sweltering!'
   end select
end program temperature
