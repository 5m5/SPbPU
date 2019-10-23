program lab_1_1
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5
   character(kind=CH_), parameter   :: CITIZEN = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file, format

   character(SURNAME_LEN, kind=CH_)                :: tmpSurname = "", Surnames(STUD_AMOUNT) = ""
   character(SURNAME_LEN, kind=CH_), allocatable   :: Citizen_Surnames(:), Guest_Surnames(:)
   
   character(INITIALS_LEN, kind=CH_)               :: tmpInitials = "", Initials(STUD_AMOUNT) = ""
   character(INITIALS_LEN, kind=CH_), allocatable  :: Citizen_Initials(:), Guest_Initials(:)

   character(kind=CH_)                             :: tmpGender = "", Gender(STUD_AMOUNT) = ""
   character(kind=CH_), allocatable                :: Citizen_Gender(:), Guest_Gender(:)
   
   character(kind=CH_)                             :: Registration(STUD_AMOUNT) = ""
   
   integer, allocatable                            :: Citizen_Pos(:), Guest_Pos(:)
   
   real(R_)                                        :: tmpAverMark = 0, Aver_Marks(STUD_AMOUNT) = 0
   real(R_), allocatable                           :: Citizen_Aver_Marks(:), Guest_Aver_Marks(:)

   logical, allocatable                            :: Is_A_Citizen(:), Is_A_Guest(:)
   integer                                         :: Citizen_Amount = 0, Guest_Amount = 0

   integer :: In, Out, IO, i, j
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
   logical :: Swap

   input_file = "../data/class.txt"
   output_file = "output.txt"
   open (file=input_file, encoding=E_, newunit=In)
      format = '(4(a, 1x), f5.2)'
      read (In, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Registration(i), &
         Aver_Marks(i), i = 1, STUD_AMOUNT)
   close (In)
   
   ! Обработка статуса чтения.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading class list."
      case(1:)
         write (Out, '(a)') "Error while reading class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select

   ! Вывод списка класса.
   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Registration(i), &
         Aver_Marks(i), i = 1, STUD_AMOUNT)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select

   Is_A_Citizen   = Registration == CITIZEN
   Citizen_Amount = Count(Is_A_Citizen)

   Citizen_Pos    = Pack(INDEXES, Is_A_Citizen)
   allocate (Citizen_Surnames(Citizen_Amount), Citizen_Initials(Citizen_Amount), &
      Citizen_Gender(Citizen_Amount), Citizen_Aver_Marks(Citizen_Amount))
   do concurrent (i = 1:Citizen_Amount)
      Citizen_Surnames(i)   = Surnames(Citizen_Pos(i))
      Citizen_Initials(i)   = Initials(Citizen_Pos(i))
      Citizen_Gender(i)     = Gender(Citizen_Pos(i))
      Citizen_Aver_Marks(i) = Aver_Marks(Citizen_Pos(i))
   end do

   Is_A_Guest   = .not. Is_A_Citizen
   Guest_Amount = STUD_AMOUNT - Citizen_Amount
   
   Guest_Pos    = Pack(INDEXES, Is_A_Guest)
   allocate (Guest_Surnames(Guest_Amount), Guest_Initials(Guest_Amount), &
      Guest_Gender(Guest_Amount), Guest_Aver_Marks(Guest_Amount))
   do concurrent (i = 1:Guest_Amount)
      Guest_Surnames(i)   = Surnames(Guest_Pos(i))
      Guest_Initials(i)   = Initials(Guest_Pos(i))
      Guest_Gender(i)     = Gender(Guest_Pos(i))
      Guest_Aver_Marks(i) = Aver_Marks(Guest_Pos(i))
   end do

   ! Сортировка списка жителей по среднему баллу методом пузырька.
   do i = Citizen_amount, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         Swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if (Citizen_Aver_Marks(j) < Citizen_Aver_Marks(j+1)) then
            Swap = .true.
         else if (Citizen_Aver_Marks(j) == Citizen_Aver_Marks(j+1)) then
            if (Citizen_Surnames(j) > Citizen_Surnames(j+1)) then
               Swap = .true.
            else if (Citizen_Surnames(j)==Citizen_Surnames(j+1) .and. Citizen_Initials(j)>Citizen_Initials(j+1)) then
               Swap = .true.
            end if
         end if

         if (Swap) then
            tmpSurname              = Citizen_Surnames(j+1)
            Citizen_Surnames(j+1)   = Citizen_Surnames(j)
            Citizen_Surnames(j)     = tmpSurname

            tmpInitials             = Citizen_Initials(j+1)
            Citizen_Initials(j+1)   = Citizen_Initials(j)
            Citizen_Initials(j)     = tmpInitials

            tmpGender               = Citizen_Gender(j+1)
            Citizen_Gender(j+1)     = Citizen_Gender(j)
            Citizen_Gender(j)       = tmpGender

            tmpAverMark             = Citizen_Aver_Marks(j+1)
            Citizen_Aver_Marks(j+1) = Citizen_Aver_Marks(j)
            Citizen_Aver_Marks(j)   = tmpAverMark
         end if
      end do
   end do

   ! Сортировка списка гостей по среднему баллу методом пузырька.
   do i = Guest_Amount, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         Swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if (Guest_Aver_Marks(j) < Guest_Aver_Marks(j+1)) then
            Swap = .true.
         else if (Guest_Aver_Marks(j) == Guest_Aver_Marks(j+1)) then
            if (Guest_Surnames(j) > Guest_Surnames(j+1)) then
               Swap = .true.
            else if (Guest_Surnames(j)==Guest_Surnames(j+1) .and. Guest_Initials(j)>Guest_Initials(j+1)) then
               Swap = .true.
            end if
         end if

         if (Swap) then
            tmpSurname            = Guest_Surnames(j+1)
            Guest_Surnames(j+1)   = Guest_Surnames(j)
            Guest_Surnames(j)     = tmpSurname

            tmpInitials           = Guest_Initials(j+1)
            Guest_Initials(j+1)   = Guest_Initials(j)
            Guest_Initials(j)     = tmpInitials

            tmpGender             = Guest_Gender(j+1)
            Guest_Gender(j+1)     = Guest_Gender(j)
            Guest_Gender(j)       = tmpGender

            tmpAverMark           = Guest_Aver_Marks(j+1)
            Guest_Aver_Marks(j+1) = Guest_Aver_Marks(j)
            Guest_Aver_Marks(j)   = tmpAverMark
         end if
      end do
   end do

   ! Вывод отсортированного списка жителей со средним баллом.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      format = '(3(a, 1x), f5.2)'
      write (out, '(/a)') "Петербуржцы:"
      write (Out, format, iostat=IO) &
         (Citizen_Surnames(i), Citizen_Initials(i), Citizen_Gender(i), Citizen_Aver_Marks(i), i = 1, Citizen_Amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select
   
      ! Вывод отсортированного списка гостей со средним баллом.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Гости города:"
      write (Out, format, iostat=IO) (Guest_Surnames(i), Guest_Initials(i), Guest_Gender(i), &
         Guest_Aver_Marks(i), i = 1, Guest_Amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted girls list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted girls list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted girls list: ", io
   end select

end program lab_1_1
