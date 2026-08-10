! For demonstration purposes only - as an example of what not to do

program bad_ptr
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   implicit none

   call run_app()

contains
   subroutine print_array(arr)
      implicit none

      integer, dimension(:), intent(in) :: arr
      integer :: idx

      write (output_unit, '(*(I5))') (arr(idx), idx = 1, size(arr))
   end subroutine print_array

   subroutine run_app()
      implicit none

      integer, dimension(:), pointer :: p1 => null(), p2 => null()
      integer :: stat
      character(len=512) :: errmsg
      integer :: i

      write (output_unit,*) 'p1, p2 associated?', associated(p1), associated(p2)

      allocate(p1(1:10), stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Error during allocation: ' // errmsg
         error stop
      end if

      p2 => p1
      p1 = [(i, i = 1,10)]

      call print_array(p1)
      call print_array(p2)

      deallocate(p1, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit,*) 'Error during deallocation: ' // errmsg
         error stop
      end if

      write (output_unit, *) 'p1, p2 associated?', associated(p1), associated(p2)

      ! Don't do this!
      call print_array(p2)

      allocate(p1(1:2), stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Error during allocation: ' // errmsg
      end if
      p1 = [21, 22]
      call print_array(p2)
   end subroutine run_app
end program bad_ptr