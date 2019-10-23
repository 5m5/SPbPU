program exercise_7_17b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: C(:, :)
   real(R_)                :: min_pos = 0
   integer, allocatable    :: Indexes(:, :), Ind_min_pos(:, :)
   logical, allocatable    :: Mask(:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M))
      read (In, *) (C(i, :), i = 1, N)
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
   close (Out)

   allocate (Indexes(N*M, 2))
   allocate (Mask(N*M), source=.false.)
  
   !call MinPos_Imp(C, min_pos, Mask, Indexes, Ind_min_pos)
   call MinPos(C, min_pos, Mask, Indexes, Ind_min_pos)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, f6.2)') "Наименьшие значения:", min_pos
      write (Out, '(2i3)') (Ind_min_pos(i, :), i = 1, UBound(Ind_min_pos, 1))
   close (Out)

contains
   ! Чистая подпрограмма в императивном стиле.
   pure subroutine MinPos_Imp(C, min_pos, Mask, Indexes, Ind_min_pos)
      real(R_), intent(in)    :: C(:, :)
      real(R_), intent(out)   :: min_pos
      integer, intent(out)    :: Indexes(:, :)
      integer, allocatable, intent(out) :: Ind_min_pos(:, :)
      logical, intent(out)    :: Mask(:)

      integer N_min_pos, i, j

      ! Формирование двумерного массива индексов:
      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      ! Поиск наименьшего
      min_pos = C(1,1)
      do j = 1, M
         do i = 1, N
            if (min_pos > C(i, j)) &
               min_pos = C(i, j)
         end do
      end do
 
      ! Маска для наименьших элементов
      do concurrent (i = 1:N*M)
         Mask(i) = .false.
      end do
      N_min_pos = 0
      do concurrent (i = 1:N, J = 1:M, C(i, j) == min_pos)
         Mask(i+(j-1)*N) = .true.
         N_min_pos = N_min_pos + 1
      end do

      ! Формирование массива индексов, удовлетворяющих заданной маске:

      ! Размещение массивов индексов.
      allocate(Ind_min_pos(N_min_pos, 2))
      ! Упаковка массива индексов.
      j = 1
      do i = 1, N*M
         if (Mask(i)) then
            Ind_min_pos(j, :) = Indexes(i, :)
            j = j + 1
         end if
      end do
   end subroutine MinPos_Imp

    ! Чистая подпрограмма в регулярном стиле.
   pure subroutine MinPos(C, min_pos, Mask, Indexes, Ind_min_pos)
      real(R_), intent(in)    :: C(:, :)
      real(R_), intent(out)   :: min_pos
      integer, intent(out)    :: Indexes(:, :)
      integer, allocatable, intent(out) :: Ind_min_pos(:, :)
      logical, intent(out)    :: Mask(:)

      integer N_min_pos, i, j

      ! Формирование двумерного массива индексов:
      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

 
      ! Поиск наименьшего элемента
      min_pos = MinVal(C)
      ! Маска для наименьших элементов
      Mask        = [C == min_pos]
      N_min_pos   = Count(Mask)

      ! Формирование массива индексов, удовлетворяющих заданной маске:

      ! Размещение массивов индексов.
      allocate(Ind_min_pos(N_min_pos, 2))
      ! Упаковка массива индексов по каждой из координат.
      Ind_min_pos(:, 1) = Pack(Indexes(:, 1), Mask)
      Ind_min_pos(:, 2) = Pack(Indexes(:, 2), Mask)
   end subroutine MinPos
end program exercise_7_17b
