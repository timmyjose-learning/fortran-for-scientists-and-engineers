! Rules:
! i) `deallocate` automatically nullifies.
! ii). Deallocate once; nullify all
! iii). Every `allocate` must have a corresponding `deallocate`

program mem_leak
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, dimension(:), pointer :: p1 => null(), p2 => null()
      integer :: i
      integer :: stat
      character(len=512) :: errmsg

      write (output_unit, *) 'Are p1 and p2 associated? ', associated(p1), associated(p2)

      allocate(p1(10), stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to allocate p1: ' // errmsg
         error stop
      end if

      p1 = [(i, i = 1, 10)]
      call print_array(p1)

      allocate(p2(1:10), stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to allocate p2: ' // errmsg
         error stop
      end if

      p2 = [(i*i, i = 1, 10)]
      call print_array(p2)

      write (output_unit, *) 'Are p1 and p2 associated? ', associated(p1), associated(p2)

      ! Uncomment this block of code to fix the memory leak
      !block
      !   deallocate(p2, stat=stat, errmsg=errmsg)
      !   if (stat /= 0) then
      !      write (error_unit, *) 'Failed to deallocate: ' // errmsg
      !      error stop
      !   end if
      !end block

      ! memory leak since p2 was not deallocated before re-assignment
      p2 => p1
      call print_array(p2)

      deallocate(p1, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to deallocate: '  // errmsg
         error stop
      end if

      nullify(p2)

   end subroutine run_app

   subroutine print_array(ptr)
      implicit none

      integer, dimension(:), pointer, intent(in) :: ptr
      integer :: i

      write (output_unit, '(*(I5))') (ptr(i), i = 1, size(ptr))
   end subroutine print_array
end program mem_leak