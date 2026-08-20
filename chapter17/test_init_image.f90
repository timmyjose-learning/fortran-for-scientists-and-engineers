program test_init_image
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit, stat_stopped_image
   implicit none

   integer, codimension[*] :: a
   integer :: i
   integer :: m
   integer :: stat
   character(len=512) :: errmsg

   if (this_image() == 1) then
      write (output_unit, *) 'Enter a number'
      read (input_unit, *) m

      do i = 1, num_images()
         a[i] = i * m
      end do
   end if

   sync all (stat=stat, errmsg=errmsg)
   if (stat == stat_stopped_image) then
      write (error_unit, *) 'One or more iimages has stopped'
   else if (stat /= 0) then
      write (error_unit, *) 'Error: ' // errmsg
   end if

   write (output_unit, '(A,I0,A,I0)') 'Result from image', this_image(), ' is ', a
end program test_init_image