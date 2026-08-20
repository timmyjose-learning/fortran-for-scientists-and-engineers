program hello_world
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   write (output_unit, *) 'Hello from image ', this_image(), ' out of ', num_images(), ' images'
end program hello_world
