module Matrix_process
   use Environment
   
   implicit none
contains
   pure subroutine Prod(B, C, result)
   real(R_), intent(inout)  :: B(:, :)
   real(R_), intent(inout)     :: C(:), result(:)

   result = matmul(B, C)

   end subroutine Prod

end module Matrix_process
