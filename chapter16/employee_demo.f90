module employee_m
   implicit none
   private

   type :: employee_t
      private
      character(len=:), allocatable :: first_name
      character(len=:), allocatable :: last_name
      character(len=:), allocatable :: ssn
      real :: pay = 0.0

   contains
      procedure :: set_name => set_name_fn
      procedure :: set_ssn => set_ssn_fn
      procedure :: get_first_name => get_first_name_fn
      procedure :: get_last_name => get_last_name_fn
      procedure :: get_ssn => get_ssn_fn
      procedure :: calc_pay => calc_pay_fn
   end type employee_t

   public :: employee_t

contains
   subroutine set_name_fn(this, fname, lname)
      implicit none

      class(employee_t), intent(inout) :: this
      character(len=*), intent(in) :: fname
      character(len=*), intent(in) :: lname

      this%first_name = fname
      this%last_name = lname
   end subroutine set_name_fn

   subroutine set_ssn_fn(this, ssn)
      implicit none

      class(employee_t), intent(inout) :: this
      character(len=*), intent(in) :: ssn

      this%ssn = ssn
   end subroutine set_ssn_fn

   function get_first_name_fn(this) result(fname)
      implicit none

      class(employee_t), intent(in) :: this
      character(len=:), allocatable :: fname

      fname = this%first_name
   end function get_first_name_fn

   function get_last_name_fn(this) result(lname)
      implicit none

      class(employee_t), intent(in) :: this
      character(len=:), allocatable :: lname

      lname = this%last_name
   end function get_last_name_fn

   function get_ssn_fn(this) result(ssn)
      implicit none

      class(employee_t), intent(in) :: this
      character(len=:), allocatable :: ssn

      ssn = this%ssn
   end function get_ssn_fn

   function calc_pay_fn(this, hours) result(pay)
      implicit none

      class(employee_t), intent(in) :: this
      integer, intent(in) :: hours
      real :: pay

      if (.false.) then
         write (*, *) hours
      end if

      pay = 0.0
   end function calc_pay_fn
end module employee_m

module salaried_employee_m
   use employee_m, only: employee_t
   implicit none
   private

   type, extends(employee_t) :: salaried_employee_t
      private
      real :: salary = 0.0

   contains
      procedure :: set_salary => set_salary_sub
      procedure :: calc_pay => salaried_calc_pay_fn !override
   end type salaried_employee_t

   public :: salaried_employee_t

contains
   subroutine set_salary_sub(this, salary)
      implicit none

      class(salaried_employee_t), intent(inout) :: this
      real, intent(in) :: salary

      this%salary = salary
   end subroutine set_salary_sub

   function salaried_calc_pay_fn(this, hours) result(pay)
      implicit none

      class(salaried_employee_t), intent(in) :: this
      integer, intent(in) :: hours
      real :: pay

      if (.false.) then
         write (*, *) hours
      end if

      pay = this%salary
   end function salaried_calc_pay_fn
end module salaried_employee_m

module hourly_employee_m
   use employee_m, only: employee_t
   implicit none
   private

   type, extends(employee_t) :: hourly_employee_t
      private
      real :: rate = 0.0

   contains
      procedure :: set_rate => set_rate_fn
      procedure :: calc_pay => hourly_calc_pay_fn
   end type hourly_employee_t

   public :: hourly_employee_t

contains
   subroutine set_rate_fn(this, rate)
      implicit none

      class(hourly_employee_t), intent(inout) :: this
      real, intent(in) :: rate

      this%rate = rate
   end subroutine set_rate_fn

   function hourly_calc_pay_fn(this, hours) result(pay)
      implicit none

      class(hourly_employee_t), intent(in) :: this
      integer, intent(in) :: hours
      real :: pay

      pay = real(hours) * this%rate
   end function hourly_calc_pay_fn
end module hourly_employee_m

program employee_demo
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use employee_m, only: employee_t
   use salaried_employee_m, only: salaried_employee_t
   use hourly_employee_m, only: hourly_employee_t
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      class(employee_t), pointer :: emp
      type(salaried_employee_t), pointer :: sal_emp
      type(hourly_employee_t), pointer :: hourly_emp
      integer :: stat
      character(len=512) :: errmsg

      allocate(sal_emp, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to allocate sal_emp: ' // errmsg
         error stop
      end if

      call sal_emp%set_name(lname='Robertson', fname='Bob')
      call sal_emp%set_ssn('123456789')
      call sal_emp%set_salary(25000.00)
      write (output_unit, *) sal_emp%calc_pay(100)

      allocate(hourly_emp, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to allocate hourly_emp: ' // errmsg
         error stop
      end if
      call hourly_emp%set_name('Newsom', 'Gary')
      call hourly_emp%set_ssn('213443340')
      call hourly_emp%set_rate(90.0)
      write (output_unit, *) hourly_emp%calc_pay(100)

      ! now via the base class pointer
      emp => sal_emp
      write (output_unit, *) emp%calc_pay(100)

      emp => hourly_emp
      write (output_unit, *) emp%calc_pay(100)

      ! deallocate
      deallocate(sal_emp, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to deallocate sal_emp: ' // errmsg
      end if

      deallocate(hourly_emp, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to deallocate hourly_emp: ' // errmsg
      end if

   end subroutine run_app
end program employee_demo