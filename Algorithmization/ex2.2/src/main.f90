program exercise_2_2
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: a = 0, b = 0, c = 0
   complex(R_)             :: x(2)

   open (file=input_file, newunit=In)
      read (In, *) a, b, c
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(4(a, f0.2/))") "a = ", a, "b = ", b, "c = ", c
   close (Out)
   
   x = F(a, b, c)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(f0.2, SP, f0.2, 'i')") x
   close (Out)

contains
   ! Чистая функция.
   pure function F(a, b, c)
      complex(R_) F(2)
      real(R_) a, b, c, D
      intent(in)  a, b, c
 
      D = b**2 - 4*a*c
      if(D == 0.0) then
         F = -b / (2 * a)
      elseif(D > 0.0) then
         F(1) = (-b + sqrt(D)) / (2 * a)
         F(2) = (-b - sqrt(D)) / (2 * a)
      else
         F(1) = (-b + sqrt(cmplx(D))) / (2 * a)
         F(2) = (-b - sqrt(cmplx(D))) / (2 * a)
      end if
   end function F
   
end program exercise_2_2
