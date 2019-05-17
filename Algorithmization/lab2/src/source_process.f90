module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Source_IO

   implicit none

contains
   pure function String_Contains(String, Characters) result(flag)
      logical                       :: flag
      type(SourceLine), intent(in)  :: String
      type(SourceLine), intent(in)  :: Characters
      integer                       :: tmp

      tmp = verify(String%String, Characters%String)
      if(tmp == 0) then
         flag = .TRUE.
      else
         flag = .FALSE.
      end if
      
   end function String_Contains

end module Source_process
