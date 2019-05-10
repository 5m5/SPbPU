module Vector_IO
   use Environment
   
   implicit none
contains
   ! Чтение вектора.
   function ReadVector(input_file) result(B)
      character(*), intent(in)   :: input_file
      real(R_), allocatable      :: B(:)

      integer :: In = 0, N = 0

      open (file=input_file, newunit=In)
         read (In, *) N
         allocate (B(N))
         read (In, *) B
      close (In)
   end function ReadVector
  
   ! Вывод вектора.
   subroutine OutputVector(output_file, B)
      character(*), intent(in)   :: output_file
      real(R_), intent(in)       :: B(:)

      integer :: Out = 0

      open (file=output_file, encoding=E_, newunit=Out, position="append")
         write (Out, '(a)') "Вектор:"
         write (Out, '(f0.2)') B
      close (Out)
   end subroutine OutputVector

end module Vector_IO
