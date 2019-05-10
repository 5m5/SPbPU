program exercise_7_1b
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0
   real(R_), allocatable   :: A(:)

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//M//"f6.2)") A
   close (Out)
   
   call Sort(A)
  
   open (file=output_file, encoding=E_, newunit=Out, position="append")
      write (Out, "(" // M // "(f0.2, 1x))") A
   close (Out)

contains
   ! Чистая подпрограмма в регулярном стиле.
   pure subroutine Sort(A)
      real(R_), intent(inout) :: A(:)

      real(R_) :: tmp
      integer  :: i, MinInd

      do i = 1, Size(A)-1
         MinInd = MinLoc(A(i:), 1) + i-1
         if (i /= MinInd) then
            tmp       = A(i)
            A(i)      = A(MinInd)
            A(MinInd) = tmp
         end if
      end do
   end subroutine Sort
end program exercise_7_1b
