program exercise_3_1b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: P = 1
   real(R_), allocatable   :: B(:) 

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (B(N))
      read (In, *) B
   close (In)

   P = ProdImp(B)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"f6.2)") B
      write (Out, *)
      write (Out, "('Product = ', f0.2)") P
   close (Out)

contains
   
   pure function ProdImp(A) result(Prod)
      real(R_)    Prod, A(:)
      intent(in)  A

      Prod = Sum(A**2)
   end function ProdImp
end program exercise_3_1b
