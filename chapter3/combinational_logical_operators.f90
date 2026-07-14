program combinational_logical_operators
   implicit none

   call and_demo()
   call or_demo()
   call not_demo()
   call eqv_demo()
   call neqv_demo()

contains
   subroutine and_demo()
      write (*, *) '.AND. operator'
      write (*, *) 'F and F = ', .false. .and. .false.
      write (*, *) 'F and T = ', .false. .and. .true.
      write (*, *) 'T and F = ', .true. .and. .false.
      write (*, *) 'T and T = ', .true. .and. .true.
   end subroutine and_demo

   subroutine or_demo()
      write (*, *) '.OR. operator'
      write (*, *) 'F or F = ', .false. .or. .false.
      write (*, *) 'F or T = ', .false. .or. .true.
      write (*, *) 'T or F = ', .true. .or. .false.
      write (*, *) 'T or T = ', .true. .or. .true.
   end subroutine or_demo

   subroutine not_demo()
      write (*, *) '.NOT. demo'
      write (*, *) 'NOT F = ',.not. .false.
      write (*, *) 'NOT T = ',.not. .true.
   end subroutine not_demo

   subroutine eqv_demo()
      write (*, *) '.EQV. demo'
      write (*, *) 'F EQV F = ', .false. .eqv. .false.
      write (*, *) 'F EQV T = ', .false. .eqv. .true.
      write (*, *) 'T EQV F = ', .true. .eqv. .false.
      write (*, *) 'T EQV T = ', .true. .eqv. .true.
   end subroutine eqv_demo

   subroutine neqv_demo()
      write (*, *) '.NEQV. demo'
      write (*, *) 'F NEQV F = ', .false. .neqv. .false.
      write (*, *) 'F NEQV T = ', .false. .neqv. .true.
      write (*, *) 'T NEQV F = ', .true. .neqv. .false.
      write (*, *) 'T NEQV T = ', .true. .neqv. .true.
   end subroutine neqv_demo
end program combinational_logical_operators
