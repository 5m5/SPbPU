program exercise_5_2
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   integer, allocatable    :: Z(:)
   logical, allocatable    :: Mask(:)
   integer, allocatable    :: Indexes(:), Ind_Extremes(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N-2))
      read (In, *) Z
   close (In)

   allocate(Mask(N))
   allocate (Indexes(N))

   call LocalExtremes(Z, Mask, Indexes, Ind_Extremes)

   Mask = Z(2: N-1) < Z(1: N-2) .and. Z(2: N-1) < Z(3:N)
   print *, Mask

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(i0, 1x))") Z
      write (Out, '(a)') "Индексы экстремумов:"
      write (Out, "("//N//"(i0, 1x))") Ind_Extremes
   close (Out)

contains
   pure subroutine LocalExtremes(Z, Mask, Indexes, Ind_Extremes)
      integer     Z(:)
      logical     Mask(:)
      integer, intent(out)              :: Indexes(:)
      integer, allocatable, intent(out) :: Ind_Extremes(:)
      intent(in)  Z
      intent(out) Mask

      integer N_min_pos, i

      Indexes(:) = [(i, i = 1, N)]

      Mask = Z(2: N-1) < Z(1: N-2) .and. Z(2: N-1) < Z(3:N)

      N_min_pos   = Count(Mask)
      allocate(Ind_Extremes(N_min_pos))

      Ind_Extremes(:) = Pack(Indexes(:), Mask)
   end subroutine LocalExtremes
end program exercise_5_2
