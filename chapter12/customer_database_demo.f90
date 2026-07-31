module error_m
   implicit none
   private

   integer, parameter :: ERR_OK = 0
   integer, parameter :: ERR_IO = 1
   integer, parameter :: ERR_ALLOC = 2
   integer, parameter :: ERR_PARSE = 3

   type :: result_t
      integer :: code = ERR_OK
      character(len=:), allocatable :: msg
   end type result_t

   public :: result_t, ERR_OK, ERR_IO, ERR_ALLOC, ERR_PARSE
end module error_m

module types_m
   implicit none

   type :: string_t
      character(len=:), allocatable :: str
   end type string_t

   type :: person_info_t
      type(string_t) :: first_name
      type(string_t) :: middle_initial
      type(string_t) :: last_name
      type(string_t) :: street
      type(string_t) :: city
      type(string_t) :: state
      integer :: zip

   contains
      procedure write_person_info_t
         generic :: write(formatted) => write_person_info_t
      end type person_info_t

      private

      public :: person_info_t

   contains
      subroutine write_person_info_t(person, unit, iotype, vlist, iostat, iomsg)
         implicit none

         class(person_info_t), intent(in) :: person
         integer, intent(in) :: unit
         character(len=*), intent(in) :: iotype
         integer, dimension(:), intent(in) :: vlist
         integer, intent(out) :: iostat
         character(len=*), intent(inout) :: iomsg

         if (.false.) then
            if (len(iotype) == 0) return
            if (size(vlist) == 0) return
         end if

         write (unit, '(6(A,1X),I5,/)', iostat=iostat, iomsg=iomsg) &
            person%first_name%str, &
            person%middle_initial%str, &
            person%last_name%str, &
            person%street%str, &
            person%city%str, &
            person%state%str, &
            person%zip
      end subroutine write_person_info_t
   end module types_m

   module database_m
      use error_m, only: result_t, ERR_OK, ERR_PARSE, ERR_ALLOC, ERR_IO
      use types_m, only: person_info_t
      implicit none
      private

      interface
         module subroutine load_database(file, persons, res)
            implicit none

            character(len=*), intent(in) :: file
            type(person_info_t), dimension(:), allocatable, intent(out) :: persons
            type(result_t), intent(out) :: res
         end subroutine load_database
      end interface

      public :: load_database
   end module database_m

   submodule (database_m) database_impl_m
      use, intrinsic :: iso_fortran_env, only: iostat_end
      implicit none

   contains
      module procedure load_database
         character(len=1024) :: line
         integer :: unit
         integer :: num_recs = 0
         integer :: iostat
         character(len=512) :: iomsg
         integer :: stat
         character(len=512) :: errmsg
         integer :: idx
         type(person_info_t) :: person

         ! find the number of records
         open(newunit=unit, file=file, status='old', action='read', iostat=iostat, iomsg=iomsg)

         if (iostat /= 0) then
            res%code = ERR_IO
            res%msg = 'While opening the file: ' // iomsg
            return
         end if

         do
            read (unit, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat == iostat_end) exit

            if (iostat /= 0) then
               res%code = ERR_IO
               res%msg = 'While reading the file: ' // iomsg
               close(unit)
               return
            end if

            num_recs = num_recs + 1
         end do

         ! allocate the array
         allocate(persons(num_recs), stat=stat, errmsg=errmsg)

         if (stat /= 0) then
            res%code = ERR_ALLOC
            res%msg = 'While allocating the array: ' // errmsg
            close(unit)
            return
         end if

         ! load the actual data into the array
         rewind(unit=unit)

         do idx = 1, num_recs
            read (unit, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
               res%code = ERR_IO
               res%msg = 'While reading a line from the file: ' // iomsg
               close(unit)
               return
            end if

            person = parse_person_info_t(line, res)

            if (res%code /= ERR_OK) then
               close(unit)
               return
            end if

            persons(idx) = person
         end do

         close(unit)
      end procedure load_database

      ! exoects the input to be in CSV format
      function parse_person_info_t(line, res) result(person)
         implicit none

         character(len=*), intent(inout) :: line
         type(result_t), intent(out) :: res
         type(person_info_t) :: person
         integer :: comma_pos = 0
         integer :: field_idx = 1
         integer :: iostat
         character(len=512) :: iomsg

         comma_pos = 0
         field_idx = 1

         do
            comma_pos = index(line, ',')
            if (comma_pos == 0 .and. field_idx /= 7) then
               res%code = ERR_PARSE
               res%msg = 'Invalid CSV data'
               return
            end if

            select case(field_idx)
             case (1)
               person%first_name%str = line(:comma_pos - 1)
               line = line(comma_pos + 1:)
             case (2)
               person%middle_initial%str = line(:comma_pos - 1)
               line = line(comma_pos + 1:)
             case (3)
               person%last_name%str = line(:comma_pos - 1)
               line = line(comma_pos + 1:)
             case (4)
               person%street%str = line(:comma_pos - 1)
               line = line(comma_pos + 1:)
             case (5)
               person%city%str = line(:comma_pos - 1)
               line = line(comma_pos + 1:)
             case (6)
               person%state%str = line(:comma_pos - 1)
               line = line(comma_pos + 1:)
             case (7)
               read (line, *, iostat=iostat, iomsg=iomsg) person%zip
               if (iostat /= 0) then
                  res%code = ERR_PARSE
                  res%msg = iomsg
                  return
               end if
               line = ''
             case default
               ! ignore, we've probably parsed everything we need
               exit
            end select

            field_idx = field_idx + 1

            if (field_idx > 7) exit
         end do
      end function parse_person_info_t
   end submodule database_impl_m

   module sort_m
      use types_m, only: person_info_t
      implicit none
      private

      abstract interface
         logical function binary_cmp_fn(p1, p2)
            ! Inside an abstract interface, parent types etc. are not included by default due to scoping rules.
            ! Use `import` to bring them into scope.
            import :: person_info_t
            implicit none

            type(person_info_t), intent(in) :: p1, p2
         end function binary_cmp_fn
      end interface

      public :: sort, by_last_name, by_city, by_zip

   contains
      ! selection sort
      subroutine sort(persons, cmp_fn)
         implicit none

         type(person_info_t), dimension(:), intent(inout) :: persons
         procedure(binary_cmp_fn) :: cmp_fn
         integer :: i, j, max_idx = 0
         type(person_info_t) :: temp

         do i = 1, size(persons) - 1
            max_idx = i
            do j = i + 1 , size(persons)
               if (cmp_fn(persons(j), persons(max_idx))) then
                  max_idx = j
               end if
            end do

            if (max_idx /= i) then
               temp = persons(max_idx)
               persons(max_idx) = persons(i)
               persons(i) = temp
            end if
         end do
      end subroutine sort

      logical function by_last_name(p1, p2)
         implicit none

         type(person_info_t), intent(in) :: p1, p2

         by_last_name = lle(p1%last_name%str, p2%last_name%str)
      end function by_last_name

      logical function by_city(p1, p2)
         implicit none

         type(person_info_t), intent(in) :: p1, p2
         by_city = lle(p1%city%str, p2%city%str)
      end function by_city

      logical function by_zip(p1, p2)
         implicit none

         type(person_info_t), intent(in) :: p1, p2
         by_zip = p1%zip <= p2%zip
      end function by_zip
   end module sort_m

   program customer_database_demo
      use, intrinsic :: iso_fortran_env, only: output_unit, input_unit, error_unit
      use error_m, only: result_t, ERR_OK
      use types_m, only: person_info_t
      use database_m, only: load_database
      use sort_m, only: sort, by_last_name, by_city, by_zip
      implicit none

      call run_app

   contains
      subroutine run_app()
         implicit none

         type(person_info_t), dimension(:), allocatable :: persons
         type(result_t) :: res
         character(len=512) :: filename
         integer :: choice

         write (output_unit, *) 'Enter the database file name'
         read (input_unit, *) filename

         call load_database(filename, persons, res)

         if (res%code /= ERR_OK) then
            write (error_unit, *) 'While loading database: ' // res%msg
            return
         end if

         do
            write (output_unit, *) 'Enter your sorting choice:'
            write (output_unit, '(T5,A)') '1 -- by last name'
            write (output_unit, '(T5,A)') '2 -- by city'
            write (output_unit, '(T5,A)') '3 -- by zip code'
            write (output_unit, '(T5,A)') '4 -- quit'

            read (input_unit, *) choice

            select case (choice)
             case (1)
               call sort(persons, by_last_name)
               call print_persons(persons)
             case (2)
               call sort(persons, by_city)
               call print_persons(persons)
             case (3)
               call sort(persons, by_zip)
               call print_persons(persons)
             case (4)
               exit
             case default
               write (error_unit, *) 'Invalid option. Try again.'
               cycle
            end select
         end do
      end subroutine

      subroutine print_persons(persons)
         implicit none

         type(person_info_t), dimension(:), intent(in) :: persons
         integer :: i

         write (output_unit, *) (persons(i), i = 1, size(persons))
      end subroutine print_persons
   end program customer_database_demo