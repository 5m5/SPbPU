program exercise_7_27
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i
   real(R_), allocatable   :: B(:,:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (B(M, N))
      read (In, *) B
   close (In)

   do concurrent (i = 1:M-1:2)
      B(i, :) = B(i, :) + B(i+1, :)
      B(i+1, :) = B(i, :) - B(i+1, :)
      B(i, :) = B(i, :) - B(i+1, :)
   end do

   ! Вывод данных.
   open (file=output_file, encoding=E_, position='rewind', newunit=Out)
       write (Out, '('//M//'f6.2)') B
   close (Out)
   
contains
   pure subroutine Swap(B, M)
      real(R_), intent(inout) :: B(:, :)
      integer, intent(in)     :: M

      integer i

      do concurrent (i = 1:M-1:2)
         B(i, :) = B(i, :) + B(i+1, :)
         B(i+1, :) = B(i, :) - B(i+1, :)
         B(i, :) = B(i, :) - B(i+1, :)
      end do

   end subroutine Swap
end program exercise_7_27
