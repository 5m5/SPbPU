program exercise_7_7
   use Environment

   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, N = 0, M = 0, L = 0, i = 0, j = 0
   real(R_), allocatable      :: C(:, :, :)
   character(:), allocatable  :: fmt
   real(R_)                   :: s = 0, prod = 1, q = 0

   open (file=input_file, newunit=In) ! С encoding=E_ может не работать на некоторых компиляторах.
      read (In, *) N, M, L
      allocate (C(N, M, L))
      ! Запись в память по строкам.
      read (In, *) ((C(i, :, j), i = 1, N), j = 1, L)
   close (In)

   do i = 1, N
      prod = 1
      do j = 1, M
         s = Sum(C(i,j,:))
         prod = prod * s
      end do
      q = q + prod
   end do
   print *, q

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(/, "Трёхмерная матрица:")')
      ! Программирование формата (O(Pf7.2, /)) для вывода O строк из P вещественных чисел
      ! и пустой строки на конце.
      fmt = "("//N//"("//M//"f7.2, /))"
      write (Out, fmt) ((C(i, :, j), i = 1, N), j = 1, L)
      write (Out, '(a, T3, "= ", f0.2)') "q", q
   close (Out)
end program exercise_7_7

