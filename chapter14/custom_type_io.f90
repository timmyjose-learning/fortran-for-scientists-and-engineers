module point_m
   implicit none

   type :: point_t
      real :: x
      real :: y
   contains
      procedure, pass(this) :: read_fmt
      procedure, pass(this) :: read_unfmt
      procedure, pass(this) :: write_fmt
      procedure, pass(this) :: write_unfmt
      generic :: read(formatted) => read_fmt
      generic :: read(unformatted) => read_unfmt
      generic :: write(formatted) => write_fmt
      generic :: write(unformatted) => write_unfmt
   end type point_t

   private :: read_fmt, read_unfmt, write_fmt, write_unfmt
   public :: point_t

contains
   subroutine read_fmt(this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(point_t), intent(inout) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, dimension(:), intent(in) :: v_list
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      select case(iotype)
       case ('LISTDIRECTED', 'NAMELIST')
         read (unit, *, iostat=iostat, iomsg=iomsg) this%x, this%y
       case default
         ! DT
         block
            integer :: width, decimals
            character(len=64) :: fmt

            width = 8
            decimals = 3

            if (size(v_list) >= 1) width = v_list(1)
            if (size(v_list) >= 2) decimals = v_list(2)

            write (fmt, '("(F",I0,".",I0,",1X,F",I0,".",I0,")")') width, decimals, width, decimals
            read (unit, fmt, iostat=iostat, iomsg=iomsg) this%x, this%y
         end block
      end select
   end subroutine read_fmt

   subroutine read_unfmt(this, unit, iostat, iomsg)
      implicit none

      class(point_t), intent(inout) :: this
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      read (unit, iostat=iostat, iomsg=iomsg) this%x, this%y
   end subroutine read_unfmt

   subroutine write_fmt(this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(point_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, dimension(:), intent(in) :: v_list
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      select case(iotype)
       case ('LISTDIRECTED', 'NAMELIST')
         write (unit, *, iostat=iostat, iomsg=iomsg) this%x, this%y
       case default
         ! DT
         block
            integer :: width, decimals
            character(len=64) :: fmt

            width = 8
            decimals = 3

            if (size(v_list) >= 1) width = v_list(1)
            if (size(v_list) >= 2) decimals = v_list(2)

            write (fmt, '("(F",I0,".",I0,",1X,F",I0,".",I0,")")') width, decimals, width, decimals
            write (unit, fmt, iostat=iostat, iomsg=iomsg) this%x, this%y
         end block
      end select
   end subroutine write_fmt

   subroutine write_unfmt(this, unit, iostat, iomsg)
      implicit none

      class(point_t), intent(in) :: this
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, iostat=iostat, iomsg=iomsg) this%x, this%y
   end subroutine write_unfmt
end module point_m

program custom_type_io
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   use point_m, only: point_t
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: iostat
      character(len=512) :: iomsg
      integer :: unit

      ! unformatted I/O demo - unformatted only works with files since
      ! output_unit and input_unit are both formatted by default
      block
         type(point_t) :: p1, p2

         p1%x = 10.0
         p1%y = -20.0

         open (newunit=unit, status='scratch', action='readwrite', form='unformatted', iostat=iostat, iomsg=iomsg)
         call check_stat(iostat, iomsg)

         write (unit) p1
         flush (unit=unit, iostat=iostat, iomsg=iomsg)
         call check_stat(iostat, iomsg)

         rewind (unit=unit, iostat=iostat, iomsg=iomsg)
         call check_stat(iostat, iomsg)

         read (unit) p2
         write (output_unit, *) p2%x, p2%y
      end block

      ! formatted I/O demo
      block
         type(point_t) :: p1, p2

         ! list-directed
         write (output_unit, *) 'Enter the point (list-directed demo)'
         read (input_unit, *) p1
         write (output_unit, *) p1

         ! DT
         write (output_unit, *) 'Enter the next point (DT demo)'
         read (input_unit, '(DT"point_t"(8,3))') p2
         write (output_unit, '(DT"point_t"(8,3))') p2
      end block
   end subroutine run_app

   subroutine check_stat(iostat, iomsg)
      implicit none

      integer, intent(in) :: iostat
      character(len=*), intent(in) :: iomsg

      if (iostat /= 0) then
         error stop 'Error: ' // iomsg
      end if
   end subroutine check_stat
end program custom_type_io