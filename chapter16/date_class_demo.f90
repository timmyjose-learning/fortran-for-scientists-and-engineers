module date_m
   implicit none
   private

   type :: date_t
      private
      integer :: day = 1
      integer :: month = 1
      integer :: year = 1900

   contains
      procedure :: set => set_date
      procedure :: is_leap_year

      procedure :: print => print_date
      generic :: write(formatted) => print
   end type date_t

   private :: set_date, print_date
   public :: date_t

contains
   subroutine set_date(this, day, month, year)
      implicit none

      class(date_t), intent(inout) :: this
      integer, intent(in) :: day
      integer, intent(in) :: month
      integer, intent(in) :: year

      this%day = day
      this%month = month
      this%year = year
   end subroutine set_date

   function is_leap_year(this) result(leap_year)
      implicit none

      class(date_t), intent(in) :: this
      logical :: leap_year

      if (mod(this%year, 400) == 0) then
         leap_year = .true.
      else if (mod(this%year, 100) /= 0 .and. mod(this%year, 4) == 0) then
         leap_year = .true.
      else
         leap_year = .false.
      end if
   end function is_leap_year

   subroutine print_date(this, unit, iotype, vlist, iostat, iomsg)
      implicit none

      class(date_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, dimension(:), intent(in) :: vlist
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      if (.false.) then
         write (unit, *) iotype
         write (unit, *) vlist
      end if

      write (unit, '("{ day = ",I5,", month = ",I5, ", year = ",I5," }")', iostat=iostat, iomsg=iomsg) this%day, this%month, this%year
   end subroutine print_date
end module date_m

program date_class_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   use date_m, only: date_t
   implicit none

   type(date_t) :: d1, d2, d3

   call d1%set(4, 1, 2016)
   call d2%set(1, 3, 1900)
   call d3%set(3, 1, 2000)

   write (output_unit, *) d1, 'is leap year? ', d1%is_leap_year()
   write (output_unit, *) d2, 'is leap year? ', d2%is_leap_year()
   write (output_unit, *) d3, 'is leap year? ', d3%is_leap_year()
end program date_class_demo