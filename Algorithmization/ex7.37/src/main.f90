program exercise_7_37
   use Environment
   
   implicit none
   character(*), parameter   :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                   :: In = 0, Out = 0, N = 0, O = 0, P = 0, Q = 0, i, j
   real(R_), allocatable     :: A(:), C(:, :, :) 
   character(:), allocatable :: fmt

   open (file=input_file, newunit=In)
      read (In, *) N, O, P, Q
      allocate (A(N))
      read (In, *) A
   close (In)

   allocate (C(O, P, Q))
   call ArrayToCube(A, C, O, P, Q)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(/, "Трёхмерная матрица:")')
      ! Программирование формата (O(Pf7.2, /)) для вывода O строк из P вещественных чисел
      ! и пустой строки на конце.
      fmt = "("//O//"("//P//"f7.2, /))"
      write (Out, fmt) ((C(i, :, j), i = 1, O), j = 1, Q)
   close (Out)

contains
   pure subroutine ArrayToCube(A, C, O, P, Q)
      real(R_), intent(in)  :: A(:)
      integer, intent (in)  :: O, P, Q
      real(R_), intent(out) :: C(:, :, :)
      integer i, j, k, z

      z = 1
      do i = 1, O
         do j = 1, P
            do k = 1, Q
               C(i, j, k) = A(z)
               z = z + 1
            end do
         end do
      end do
   end subroutine ArrayToCube
end program exercise_7_37
