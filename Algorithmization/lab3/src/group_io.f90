module Group_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: YEAR_AMOUNT   = 4

   ! Структура данных для хранения данных о студенте.
   type student
      character(SURNAME_LEN, kind=CH_)  :: Surname                    = ""
      integer                           :: Birthday_year(YEAR_AMOUNT) = 0
      type(student), pointer            :: next                       => Null()
   end type student

contains
   ! Чтение списка
   function Read_person_list(Input_File) result(Person_List)
      type(student), pointer     :: Person_List
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Person_List => Read_person(In)
      close (In)
   end function Read_person_list

   ! Чтение следующего человека
   recursive function Read_person(In) result(Stud)
      type(student), pointer  :: Stud
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Stud)
      format = '((a, 1x), ' // YEAR_AMOUNT // 'i1)'
      read (In, format, iostat=IO) stud%Surname, stud%Birthday_year
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Stud%next => Read_person(In)
      else
         deallocate (Stud)
         nullify (Stud)
      end if
   end function Read_person

   ! Вывод списка
   subroutine Output_person_list(Output_File, Person_List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(student), intent(in)  :: Person_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_student(Out, Person_List)
      close (Out)
   end subroutine Output_person_list

   recursive subroutine Output_student(Out, Stud)
      integer, intent(in)        :: Out
      type(student), intent(in)  :: Stud
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '((a, 1x), ' // YEAR_AMOUNT // 'i1)'
      write (Out, format, iostat=IO) Stud%Surname, Stud%Birthday_year
      call Handle_IO_status(IO, "writing student")
      if (Associated(Stud%next)) &
         call Output_student(Out, Stud%next)
   end subroutine Output_student
end module Group_IO 
