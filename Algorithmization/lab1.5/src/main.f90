program lab_1_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   character(kind=CH_), parameter   :: CITIZEN = Char(1055, CH_), GUEST = Char(1057, CH_)

   type(student), pointer  :: Group_List => Null(), Citizen_List => Null(), Guest_List => Null()
   integer(I_)             :: Citizen_Amount = 0, Guest_Amount = 0

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   Group_List => Read_class_list(input_file)

   if (Associated(Group_List)) then
      call Output_class_list(output_file, Group_List, "Исходный список:", "rewind")

      call Get_list_by_registration(Group_List, Citizen_List, Citizen_Amount, CITIZEN)
      call Get_list_by_registration(Group_List, Guest_List, Guest_Amount, GUEST)
   
      call Sort_class_list(Citizen_List, Citizen_Amount)
      call Sort_class_list(Guest_List, Guest_Amount)
   
      if (Associated(Citizen_List)) &
         call Output_class_list(output_file, Citizen_List, "Петербуржцы:", "append")
      if (Associated(Guest_List)) &
         call Output_class_list(output_file, Guest_List, "Гости города:", "append")
   end if

end program lab_1_5
