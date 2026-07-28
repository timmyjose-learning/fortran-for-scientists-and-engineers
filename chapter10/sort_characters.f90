module error_utils_m
   implicit none
   private

   integer, parameter :: ERR_OK = 0
   integer, parameter :: ERR_IO = 1
   integer, parameter :: ERR_ALLOC = 2

   public :: error_t, ERR_OK, ERR_IO, ERR_ALLOC

   type :: error_t
      integer :: code = ERR_OK
      character(len=:), allocatable :: msg
   end type error_t
end module error_utils_m

module sort_utils_m
   use array_utils_m, only: string_t
   implicit none
   private

   public :: sort_array

   interface
      module subroutine sort_array(arr)
         implicit none
         type(string_t), dimension(:), intent(inout) :: arr
      end subroutine sort_array
   end interface
end module sort_utils_m

submodule(sort_utils_m) sort_utils_impl_m
   implicit none

contains
   module procedure sort_array
      integer :: i, j, swap_idx
      type(string_t) :: temp

      do i = 1, size(arr, 1) - 1
         swap_idx = i
         do j = i + 1, size(arr, 1)
            if (llt(arr(j)%str, arr(swap_idx)%str)) then
               swap_idx = j
            end if
         end do

         if (swap_idx /= i) then
            temp = arr(swap_idx)
            arr(swap_idx) = arr(i)
            arr(i) = temp
         end if
      end do
   end procedure sort_array
end submodule sort_utils_impl_m

module array_utils_m
   use error_utils_m, only: error_t, ERR_IO, ERR_OK, ERR_ALLOC
   implicit none
   private

   type :: string_t
      character(len=:), allocatable :: str
   end type string_t

   public :: string_t,  read_array_from_file, print_array

   interface
      module subroutine read_array_from_file(file, arr, err)
         implicit none
         character(len=*), intent(in) :: file
         type(string_t), dimension(:), allocatable, intent(out) :: arr
         type(error_t), intent(out) :: err
      end subroutine read_array_from_file

      module subroutine print_array(arr)
         implicit none
         type(string_t), dimension(:), intent(in) :: arr
      end subroutine print_array
   end interface
end module array_utils_m

submodule(array_utils_m) array_utils_impl_m
   implicit none

contains
   module procedure read_array_from_file
      integer :: unit
      integer :: iostat
      character(len=512) :: iomsg
      integer :: nvals = 0
      character(len=512) :: line
      integer :: stat
      character(len=512) :: errmsg
      integer :: i

      ! first pass - get the number of values in the file
      open(newunit=unit, file=file, status='old', action='read', iostat=iostat, iomsg=iomsg)

      if (iostat /= 0) then
         err%code = ERR_IO
         err%msg = 'Error while opening file'
         return
      end if

      nvals = 0
      do
         read (unit, '(A)', iostat=iostat, iomsg=iomsg) line
         if (iostat /= 0) then
            exit
         end if
         nvals = nvals + 1
      end do

      if (iostat > 0) then
         err%code = ERR_IO
         err%msg = 'Error while reading file'
         close(unit)
         return
      end if

      ! allocate the array
      allocate(arr(nvals), stat=stat, errmsg=errmsg)

      if (stat /= 0) then
         err%code = ERR_ALLOC
         err%msg = 'Failed to allocate memory for array'
         close(unit)
         return
      end if

      ! rewind and read the array in
      rewind(unit=unit)

      do i = 1, nvals
         read (unit, '(A)') line
         arr(i)%str = adjustl(trim(line))
      end do

      close(unit)
   end procedure

   module procedure print_array
      integer :: i

      do i = 1, size(arr)
         write (*, '(A)') arr(i)%str
      end do
      write (*, *)
   end procedure print_array
end submodule array_utils_impl_m

program sorting_characters
   use array_utils_m, only: string_t, read_array_from_file, print_array
   use sort_utils_m, only: sort_array
   use error_utils_m, only: error_t, ERR_OK
   implicit none

   character(len=512) :: filename
   type(string_t), dimension(:), allocatable :: arr
   type(error_t) :: err

   write (*, *) 'Enter the filename'
   read (*, '(A)') filename
   filename = trim(adjustl(filename))

   call read_array_from_file(filename, arr, err)

   if (err%code /= ERR_OK) then
      write (*, *) err%msg
      stop
   end if

   write (*, *) 'Before sorting...'
   call print_array(arr)

   call sort_array(arr)
   write (*, *) 'After sorting...'
   call print_array(arr)

   if (allocated(arr)) then
      deallocate(arr)
   end if
end program sorting_characters