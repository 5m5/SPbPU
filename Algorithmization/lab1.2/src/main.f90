program lab_1_2
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5, MARKS_AMOUNT = 5
   character(kind=CH_), parameter   :: CITIZEN = Char(1055, CH_), GUEST = Char(1057, CH_)
   character(:), allocatable        :: input_file, output_file

   character(kind=CH_)  :: Surnames(STUD_AMOUNT, SURNAME_LEN)  = "", &
                           Initials(STUD_AMOUNT, INITIALS_LEN) = "", &
                           Genders(STUD_AMOUNT) = "", Registrations(STUD_AMOUNT) = ""

   character(kind=CH_), allocatable :: Citizen_Surnames(:, :), Guest_Surnames(:, :), Citizen_Gender(:)
   character(kind=CH_), allocatable :: Citizen_Initials(:, :), Guest_Initials(:, :), Guest_Gender(:)

   integer              ::  i = 0

   real(R_)             :: Aver_Marks(STUD_AMOUNT) = 0
   real(R_),allocatable :: Citizen_Aver_Marks(:), Guest_Aver_Marks(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   call Read_class_list(input_file, Surnames, Initials, Genders, Registrations, &
      Aver_Marks)

   call Output_class_list(output_file, Surnames, Initials, Genders, Registrations, &
      Aver_Marks, "Исходный список:", "rewind")

   call Get_list_by_registration(Surnames, Initials, Genders, Registrations, Aver_Marks, &
      Citizen_Surnames, Citizen_Initials, Citizen_Gender, Citizen_Aver_Marks, CITIZEN)
   call Get_list_by_registration(Surnames, Initials, Genders, Registrations, Aver_Marks, &
      Guest_Surnames, Guest_Initials, Guest_Gender, Guest_Aver_Marks, GUEST)

   call Sort_class_list(Citizen_Surnames, Citizen_Initials, Citizen_Gender, Citizen_Aver_Marks)
   call Sort_class_list(Guest_Surnames, Guest_Initials, Guest_Gender, Guest_Aver_Marks)

   call Output_class_list(output_file, Citizen_Surnames, Citizen_Initials, Citizen_Gender, &
      [(CITIZEN, i = 1, Size(Citizen_Aver_Marks))], &
      Citizen_Aver_Marks, "Петербуржцы:", "append")
   call Output_class_list(output_file, Guest_Surnames, Guest_Initials, Guest_Gender, &
      [(GUEST, i = 1, Size(Guest_Aver_Marks))], &
      Guest_Aver_Marks, "Гости города:", "append")

contains
   subroutine Read_class_list(Input_File, Surnames, Initials, Genders, Registrations, Aver_Marks)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:), Registrations(:)
      real(R_)             Aver_Marks(:)
      intent (in)          Input_File
      intent (out)         Surnames, Initials, Genders, Registrations, Aver_Marks

      integer In, IO, i
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, a, 1x, f5.2)'
         read (In, format, iostat=IO) (Surnames(i, :), Initials(i, :), Genders(i), Registrations(i), Aver_Marks(i), &
            i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "reading class list")
      close (In)
   end subroutine Read_class_list

   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Surnames, Initials, Genders, Registrations, Aver_Marks, List_name, Position)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:), Registrations(:)
      real(R_)             Aver_Marks(:)
      intent (in)          Output_File, Surnames, Initials, Genders, Registrations, Aver_Marks, List_name, Position

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, a, 1x, f5.2)'
         write (Out, format, iostat=IO) &
            (Surnames(i, :), Initials(i, :), Genders(i), Registrations(i), Aver_Marks(i), i = 1, Size(Aver_Marks))
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list

   pure subroutine Get_list_by_registration(Surnames, Initials, Genders, Registrations, &
      Aver_Marks, Registration_Surnames, Registration_Initials, Registration_Gender, &
      Registration_Aver_Marks, Registration)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:), Registrations(:)
      character(kind=CH_)  Registration_Surnames(:, :), Registration_Initials(:, :), Registration_Gender(:)
      real(R_)             Aver_Marks(:), Registration_Aver_Marks(:)
      character(kind=CH_)  Registration
      intent(in)           Surnames, Initials, Genders, Registrations, Aver_Marks, Registration
      intent(out)          Registration_Surnames, Registration_Initials, Registration_Gender, Registration_Aver_Marks
      allocatable          Registration_Surnames, Registration_Initials, Registration_Gender, Registration_Aver_Marks

      logical, allocatable :: Is_A_Registration(:)
      integer, allocatable :: Registration_Pos(:)
      integer              :: Registration_Amount, i
      integer, parameter   :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

      ! Составление логической маски, соответствующей полу.
      Is_A_Registration    = Registrations == Registration
      Registration_Amount  = Count(Is_A_Registration)

      ! Получение массивов, связынных с заданным полом.
      Registration_Pos  = Pack(INDEXES, Is_A_Registration)
      allocate (Registration_Surnames(Registration_Amount, SURNAME_LEN), &
         Registration_Initials(Registration_Amount, INITIALS_LEN), &
         Registration_Gender(Registration_Amount), Registration_Aver_Marks(Registration_Amount))
      ! Получение двумерных списков для пола.
      do concurrent (i = 1:Registration_Amount)
         Registration_Surnames(i, :) = Surnames(Registration_Pos(i), :)
         Registration_Initials(i, :) = Initials(Registration_Pos(i), :)
         Registration_Gender(i)      = Genders(Registration_Pos(i))
         Registration_Aver_Marks(i)  = Aver_Marks(Registration_Pos(i))
      end do
   end subroutine Get_list_by_registration

   pure subroutine Sort_class_list(Surnames, Initials, Genders, Aver_Marks)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
      real(R_)             Aver_Marks(:)
      intent (inout)       Surnames, Initials, Genders, Aver_Marks

      integer              i, j
      
      ! Сортировка списка класса по среднему баллу методом пузырька.
      do i = Size(Aver_Marks), 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять учащихся местами.
            if (Swap(Aver_Marks, Surnames, Initials, j)) &
               call Swap_from_current(Surnames, Initials, Genders, Aver_Marks, j)
         end do
      end do
   end subroutine Sort_class_list   
      
   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Aver_Marks, Surnames, Initials, j)
      character(kind=CH_)  Surnames(:, :), Initials(:, :)
      real(R_)             Aver_Marks(:)
      integer              j
      intent (in) :: Surnames, Initials, Aver_Marks, j

      Swap = .false.
      if (Aver_Marks(j) < Aver_Marks(j+1)) then
         Swap = .true.
      else if (Aver_Marks(j) == Aver_Marks(j+1)) then
         if (GT(Surnames(j, :), Surnames(j+1, :))) then
            Swap = .true.
         else if (All(Surnames(j, :) == Surnames(j+1, :)) .and. GT(Initials(j, :), Initials(j+1, :))) then
            Swap = .true.
         end if
      end if
   end function Swap
   
   ! Функция операции > для массивов символов.
   pure logical function GT(arr1, arr2)
      character(kind=CH_), intent(in) :: arr1(:), arr2(:)

      integer :: i 

      ! Поиск первого отличного символа или остановка на последнем символе.
      do i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
            exit
      end do
      GT = arr1(i) > arr2(i)  
   end function GT
   
   ! Перестановка местами двух эелементов списка, начиная с текущего.
   pure subroutine Swap_from_current(Surnames, Initials, Genders, Aver_Marks, j)
      character(kind=CH_)     Surnames(:, :), Initials(:, :), Genders(:)
      real(R_)                Aver_Marks(:)
      integer, intent(in)  :: j
      intent (inout)       :: Surnames, Initials, Genders, Aver_Marks
      
      character(kind=CH_)  tmpSurname(SURNAME_LEN), tmpInitials(INITIALS_LEN), tmpGenders
      real(R_)             tmpAverMark

      tmpSurname = Surnames(j+1, :)
      Surnames(j+1, :) = Surnames(j, :)
      Surnames(j, :) = tmpSurname

      tmpInitials = Initials(j+1, :)
      Initials(j+1, :) = Initials(j, :)
      Initials(j, :) = tmpInitials

      tmpGenders = Genders(j+1)
      Genders(j+1) = Genders(j)
      Genders(J) = tmpGenders

      tmpAverMark = Aver_Marks(j+1)
      Aver_Marks(j+1) = Aver_Marks(j)
      Aver_Marks(j) = tmpAverMark
   end subroutine Swap_from_current

end program lab_1_2
