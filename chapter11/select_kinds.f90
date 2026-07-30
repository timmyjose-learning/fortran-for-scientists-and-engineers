program select_kinds
   implicit none

   integer, parameter :: SGL = selected_real_kind(p=6, r=37)
   integer, parameter :: DBL = selected_real_kind(p=13, r=200)
   integer, parameter :: QUAD = selected_real_kind(p=26,r=4000)

   real(kind=SGL) :: var1 = 0.0
   real(kind=DBL) :: var2 = 0.0
   real(kind=QUAD) :: var3 = 0.0

   write (*, '("var1 (SGL): ", I5,1X,I5,1X,I0)') kind(var1), precision(var1), range(var1)
   write (*, '("var2 (DBL): ", I5,1X,I5,1X,I0)') kind(var2), precision(var2), range(var2)
   write (*, '("var3 (DBL): ", I5,1X,I5,1X,I0)') kind(var3), precision(var3), range(var3)
end program select_kinds