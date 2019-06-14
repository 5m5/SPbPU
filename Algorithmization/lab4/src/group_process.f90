module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO
   implicit none

contains
   pure recursive subroutine Get_list(Num, List)
      type(number), intent(in)        :: Num
      type(number), pointer           :: List      
      allocate (List, source=Num)
      List%next => Null()
      if (Associated(Num%next)) then
          call Get_list(Num%next, List%next)
      else if (Associated(Num%next)) then
          call Get_list(Num%next, List)
      end if

   end subroutine Get_list

   pure recursive subroutine Calculate(polynomial, res)
      type(number), intent(in) :: polynomial
      integer, intent(inout)     :: res
      res = res + polynomial%coeff**polynomial%power
      if (Associated(polynomial%next)) then

        call Calculate(polynomial%next, res)
      end if

   end subroutine Calculate

end module Group_process
