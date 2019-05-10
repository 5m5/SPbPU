module Matrix_IO
   use Environment
   
   implicit none
contains
   ! Чтение матрицы.
   function ReadMatrix(input_file) result(B)
      character(*), intent(in)   :: input_file
      real(R_), allocatable      :: B(:, :)

      integer :: In = 0, i = 0, N = 0, M = 0

      open (file=input_file, newunit=In)
         read (In, *) N, M
         allocate (B(N, M))
         read (In, *) (B(i, :), i = 1, N)
      close (In)
   end function ReadMatrix
  
   ! Вывод матрицы.
   subroutine OutputMatrix(output_file, B)
      character(*), intent(in)   :: output_file
      real(R_), intent(in)       :: B(:, :)

      integer :: Out = 0, i = 0

      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, '(a)') "Матрица:"
         write (Out, '('//UBound(B, 2)//'f6.2)') (B(i, :), i = 1, UBound(B, 1))
      close (Out)
   end subroutine OutputMatrix

end module Matrix_IO
