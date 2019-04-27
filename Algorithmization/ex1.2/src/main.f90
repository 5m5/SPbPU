program exercise_1_2 
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, K = 0, POWER = 6, I
   real(R_)                   :: XVALUE = 0, P = 0
   real(R_), allocatable      :: A(:), X(:)
   character(:), allocatable  :: fmt

   open (file=input_file, newunit=In)
      read (In, *) K
      allocate (A(K))
      read (In, *) A
      read (In, *) XVALUE
   close (In)

   allocate (X(POWER))
   do I = 1, POWER
      X(I) = XVALUE**(POWER - I)
   end do
   P = dot_product(A, X)

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, f0.2)')"
      write (Out, fmt) "p(x) = ", P
   close (Out)
end program exercise_1_2
