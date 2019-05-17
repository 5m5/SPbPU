program exercise_7_27
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i
   real(R_), allocatable   :: B(:,:), tmp(:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) M, N
      allocate (B(M, N))
      read (In, *) B
   close (In)
   allocate (tmp(M))

   open (file=output_file, encoding=E_, newunit=Out)
       write (Out, '('//M//'f6.2)') B
   close (Out)

   do concurrent (i = 1:M-1:2)
         tmp = B(i, :)
         B(i, :) = B(i+1, :)
         B(i+1, :) = tmp
   end do

   ! Вывод данных.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
       write (Out, *) "После перестановки:"
       write (Out, '('//M//'f6.2)') B
   close (Out)
   
end program exercise_7_27
