module Matrix_process
   use Environment
   
   implicit none
contains
   pure subroutine Prod(B, C, result)
   real(R_), intent(inout)  :: B(:, :)
   real(R_), intent(inout)  :: C(:), result(:)

   result = matmul(B, C)

   end subroutine Prod

   pure subroutine Prod_Imp(B, C, result)
   real(R_), intent(inout)  :: B(:, :)
   real(R_), intent(inout)  :: C(:), result(:)
   integer i, j

   result = 0
   do i = 1, size(B(:, 1))
      do j = 1, size(B(1, :))
         result(i) = result(i) + B(i, j) * C(j)
      end do
   end do

   end subroutine Prod_Imp

end module Matrix_process
