program example_3_4
   implicit none

   integer :: score
   character(len=1) :: grade

   write (*, *) 'Enter the score (0-100)'
   read (*, *) score

   grade = calculate_grade_else_if(score)
   write (*, *) 'Grade calculated using else if ', grade

   grade = calculate_grade_nested_if(score)
   write (*, *) 'Grade calculated using nested if = ', grade

contains
   function calculate_grade_else_if(s) result(g)
      implicit none

      integer, intent(in) :: s
      character(len=1) :: g

      if (s > 95) then
         g = 'A'
      else if (s > 86 .and. s <= 95) then
         g = 'B'
      else if (s > 76 .and. s <= 86) then
         g = 'C'
      else if (s > 66 .and. s <= 76) then
         g = 'D'
      else if (s > 0 .and. s <= 66) then
         g = 'F'
      end if
   end function calculate_grade_else_if

   function calculate_grade_nested_if(s) result(g)
      implicit none

      integer, intent(in) :: s
      character(len=1) :: g

      if1: if (s > 95) then
         g = 'A'
      else
         if2: if (s > 86) then
            g = 'B'
         else
            if3: if (s > 76) then
               g = 'C'
            else
               if4: if (s > 66) then
                  g = 'D'
               else
                  g = 'F'
               end if if4
            end if if3
         end if if2
      end if if1
   end function calculate_grade_nested_if
end program example_3_4
