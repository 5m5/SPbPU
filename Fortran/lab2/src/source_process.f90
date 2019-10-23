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

   pure function String_Contains_Imp(String, Characters) result(flag)
      logical                       :: flag
      type(SourceLine), intent(in)  :: String
      type(SourceLine), intent(in)  :: Characters
      integer                       :: i, j

      do i = 1, len(String%String)
         do j = 1, len(Characters%String)
            if(String%String(i:i) == Characters%String(j:j)) then
               flag = .TRUE.
               exit
            end if
            flag = .FALSE.
         end do
      end do
      
   end function String_Contains_Imp

end module Source_process
