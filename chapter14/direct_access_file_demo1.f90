program direct_access_file_demo1
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: unit
      integer :: iostat
      character(len=512) :: iomsg
      integer :: id

      open(newunit=unit, file='daf.fmt', status='replace', action='readwrite', access='direct', recl=100, form='formatted', iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      do id = 1, 100
         write (unit, '("This is record number: ",I0)', rec=id, iostat=iostat, iomsg=iomsg) id
         call check_stat(iostat, iomsg)
      end do

      flush(unit=unit, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      block
         integer :: choice
         integer :: rec_num
         character(len=100) :: rec

         do
            write (output_unit, '("Enter your choice:",/,5X,"1. View record",/,4X," 2. Exit")')
            read (input_unit, *) choice

            select case (choice)
             case (1)
               write (output_unit, '(A)', advance='no') 'Which record number? (1-100) '
               read (input_unit, *) rec_num

               if (rec_num < 1 .or. rec_num > 100) then
                  write (error_unit, *) 'Invalid record number: ' // int_to_string(rec_num)
                  cycle
               end if

               read (unit, '(A)', rec=rec_num) rec
               write (output_unit, '(A)') rec

             case (2)
               exit
             case default
               write (error_unit, *) 'Invalid choice: ' // int_to_string(choice)
               cycle
            end select
         end do
      end block

      close(unit=unit, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)
   end subroutine run_app

   function int_to_string(int) result(char)
      implicit none

      integer, intent(in) :: int
      character(len=12) :: char

      write (char, '(I0)') int
   end function int_to_string

   subroutine check_stat(iostat, iomsg)
      implicit none

      integer, intent(in) :: iostat
      character(len=*), intent(in) :: iomsg

      if (iostat /= 0) then
         error stop 'Error: ' // iomsg
      end if
   end subroutine check_stat
end program direct_access_file_demo1