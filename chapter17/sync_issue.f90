program sync_issue
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: stat
      character(len=512) :: errmsg

      errmsg = ''

      if (this_image() == 1) then
         write (output_unit, *) 'Image 1 syncing with images 2 and 3'
         sync images ([2, 3], stat=stat, errmsg=errmsg)
         call check_stat(stat, errmsg)
         write (output_unit, *) 'Image 1 post syncing'
      end if

      if (this_image() == 2) then
         write (output_unit, *) 'Image 2 syncing with image 1'
         sync images (1, stat=stat, errmsg=errmsg)
         call check_stat(stat, errmsg)
         write (output_unit, *) 'Image 2 post syncing'
      end if

      if (this_image() == 3) then
         write (output_unit, *) 'Image 3 not syncing with any other images'
      end if

      write (output_unit, '("Image ",I0," reached the end.")') this_image()
   end subroutine run_app

   subroutine check_stat(stat, errmsg)
      implicit none

      integer, intent(in) :: stat
      character(len=*), intent(in) :: errmsg

      if (stat /= 0) then
         error stop 'Error: ' // errmsg
      end if
   end subroutine check_stat
end program sync_issue