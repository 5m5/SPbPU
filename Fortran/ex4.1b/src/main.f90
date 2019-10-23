program exercise_4_1b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_)                :: x_min = 0, x_max = 0, h = 0
   real(R_), allocatable   :: X(:), F(:), Sin_X(:), Ln_X(:) 
   
   open (file=input_file, newunit=In)
      read (In, *) x_min, x_max, h
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x_min", x_min, "x_max", x_max, "h", h
   close (Out)
   
   N = Int((x_max-x_min) / h + .5_R_) + 1

   allocate (X(N), F(N), Sin_X(N), Ln_X(N))
  
   !call TabFImp(x_min, h, X, F)
   call TabF(x_min, h, X, F)
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("  X   |   f")')
      write (Out, '(f0.4, T7, "| ", f0.4)') (X(i), F(i), i = 1, N)
   close (Out)

contains
   ! Чистая функция в императивном стиле.
   pure subroutine TabFImp(x_min, h, X, F)
      real(R_)    x_min, h, X(:), F(:)
      intent(in)  x_min, h
      intent(out) X, F
      integer     N, i
      
      N = Size(X)
      X(1) = x_min
      do i = 1, N-1
         F(i) = ALOG(X(i)) + Sin(X(i))**2
         X(i+1) = X(i) + h
      end do    
      F(N) = ALOG(X(i)) + Sin(X(N))**2
   end subroutine TabFImp
   
   ! Чистая функция в регулярном стиле.
   pure subroutine TabF(x_min, h, X, F)
      real(R_)    x_min, h, X(:), F(:)
      intent(in)  x_min, h
      intent(out) X, F
      integer     i
      do concurrent (i = 1:Size(X))
         X(i) = x_min + h*(i-1)
      end do
      ! Функция закодирована по-другому, чтобы дважды не вычислять Sin(x).
      F = ALOG(X) + Sin(X)**2
   end subroutine TabF
end program exercise_4_1b
