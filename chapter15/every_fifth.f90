program every_fifth
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, dimension(:), pointer :: arr
      integer, dimension(:), pointer :: ptr
      integer :: nvals = 0

      write (output_unit, *) 'How many values?'
      read (input_unit, *) nvals

      if (nvals < 1) then
         write (error_unit, *) 'Invalid number of elements'
         error stop
      end if

      block
         integer :: ctr = 1
         integer :: idx = 1
         integer :: val
         integer :: iostat
         integer :: stat
         character(len=512) :: errmsg

         allocate(arr(nvals), stat=stat, errmsg=errmsg)
         if (iostat /= 0) then
            write (error_unit, *) 'Allocation failed: ' // errmsg
            return
         end if

         do while (ctr <= nvals)
            write (output_unit, *) 'Enter an integer'
            read (input_unit, *, iostat=iostat) val

            if (iostat /= 0) cycle
            arr(idx) = val
            idx = idx + 1
            ctr = ctr + 1
         end do
      end block

      ptr => collect_fifths(arr)
      call print_array(ptr)

      if (associated(ptr)) then
         nullify(ptr)
      end if

      if (associated(arr)) then
         deallocate(arr)
      end if
   end subroutine run_app

   function collect_fifths(p) result(fifths)
      implicit none

      integer, dimension(:), pointer, intent(in) :: p
      integer, dimension(:), pointer :: fifths
      integer :: lb, ub

      lb = lbound(p, 1)
      ub = ubound(p, 1)

      fifths => p(lb:ub:5)
   end function collect_fifths

   subroutine print_array(p)
      implicit none

      integer, dimension(:), pointer, intent(in) :: p
      integer :: i

      write (output_unit, *) (p(i), i = 1, size(p))
   end subroutine print_array
end program every_fifth