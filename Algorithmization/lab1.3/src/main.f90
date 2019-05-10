program lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: CITIZEN = Char(1055, CH_), GUEST = Char(1057, CH_)
   
   type(student)              :: Group(STUD_AMOUNT)
   type(student), allocatable :: Citizen_List(:), Guest_List(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   Citizen_List  = Pack(Group, Group%Registration == CITIZEN)
   Guest_List = Pack(Group, Group%Registration == GUEST)

   call Sort_class_list(Citizen_List)
   call Sort_class_list(Guest_List)

   call Output_class_list(output_file, Citizen_List, "Петербуржцы:", "append")
   call Output_class_list(output_file, Guest_List, "Гости города:", "append")

end program lab_1_3
