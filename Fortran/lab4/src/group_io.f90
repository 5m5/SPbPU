module Group_IO
   use Environment

   implicit none

   ! Структура данных для хранения данных о студенте.
   type number
      integer               :: coeff = 0
      integer               :: power = 0
      type(number), pointer :: next => Null()
   end type number

contains
   ! Чтение списка
   function Read_polynomial_list(Input_File) result(Polynomial_List)
      type(number), pointer      :: Polynomial_List
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Polynomial_List => Read_polynomial(In)
      close (In)
   end function Read_polynomial_list

   recursive function Read_polynomial(In) result(Numb)
      type(number), pointer   :: Numb
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Numb)
      format = '(i2, i2)'
      read (In, format, iostat=IO) numb%coeff, numb% power
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Numb%next => Read_polynomial(In)
      else
         deallocate (Numb)
         nullify (Numb)
      end if
   end function Read_polynomial

   ! Вывод списка
   subroutine Output_polynomial_list(Output_File, Polynomial_List, List_Name, Position)
      character(*), intent(in)  :: Output_File, Position, List_Name
      type(number), intent(in)  :: Polynomial_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_polynomial(Out, Polynomial_List)
      close (Out)
   end subroutine Output_polynomial_list

   recursive subroutine Output_polynomial(Out, Pol)
      integer, intent(in)        :: Out
      type(number), intent(in)   :: Pol
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(i2, i2)'
      write (Out, format, iostat=IO) Pol%coeff, Pol%power
      call Handle_IO_status(IO, "writing polynomial")
      if (Associated(Pol%next)) &
         call Output_polynomial(Out, Pol%next)
   end subroutine Output_polynomial

   subroutine writeResult(Res, Output_file, Position)
      character(*), intent(in)   :: Output_File, Position
      real(R_), intent(in)       :: Res

      integer                    :: Out
      character(:), allocatable  :: format

      format  = '(f6.2)'
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         Write(Out, '(/a)') "Result: "
         write(Out, format) Res
      close(Out)

   end subroutine writeResult
end module Group_IO 
