module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO
   implicit none

contains
   pure recursive subroutine Get_list(Stud, List)
      type(student), intent(in)        :: Stud
      type(student), pointer           :: List      
      allocate (List, source=Stud)
      List%next => Null()
      if (Associated(Stud%next)) then
          call Get_list(Stud%next, List%next)
      else if (Associated(Stud%next)) then
          call Get_list(Stud%next, List)
      end if

   end subroutine Get_list

end module Group_process
