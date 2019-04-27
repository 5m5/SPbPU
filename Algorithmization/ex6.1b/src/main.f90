program exercise_6_1b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: cos_x = 0, x = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
   !cos_x = CosXImp(x)
   cos_x = CosX(x)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "Cos(x)", cos_x, "Fortran Cos(x)", Cos(x), "Error", cos_x - Cos(x)
   close (Out)

contains
   real(R_) pure function CosX(x)
      real(R_), intent(in) :: x
      
      real(R_), parameter :: PI_2 = 8 * Atan(1._R_)
      real(R_), parameter :: RELERR = epsilon(1._R_)
      real(R_) R(4), Numerators(4), Denominators(4), n_fact, x_s, x_8
      integer  Ns(8)

      x_s = Mod(x, PI_2)
      
      Numerators = x_s ** [2, 4, 6, 8]
      Numerators = Numerators * [-1, 1, -1, 1] ! ВЕКТОРИЗАЦИЯ
      
      x_8 = Numerators(4)
      
      Denominators = [2, 2*3*4, 2*3*4*5*6, 2*3*4*5*6*7*8]
      Ns = [1, 3, 5, 7, 2, 4, 6, 8]

      R = Numerators / Denominators ! ВЕКТОРИЗАЦИЯ

      CosX = 1 + Sum(R)
      
      do while (Abs(R(4) / CosX) >= RELERR)
         Numerators = Numerators * x_8
         n_fact = Denominators(4)
         Ns = Ns + 8
         
         Denominators(1) = n_fact * Denominators(1)
         Denominators(2) = Denominators(1) * Denominators(2) ! == (n+4)! == (n+2)! * (n+3)*(n+4)
         Denominators(3) = Denominators(2) * Denominators(3) ! == (n+6)! == (n+4)! * (n+5)*(n+6)
         Denominators(4) = Denominators(3) * Denominators(4) ! == (n+8)! == (n+6)! * (n+7)*(n+8)
         
         R = Numerators / Denominators

         ! Прибавление очередных 4-ёх членов к накапливаемой сумме.
         CosX = CosX + Sum(R)
         if (Abs(R(4) / CosX) < RELERR) exit
      end do
   end function CosX 
end program exercise_6_1b

