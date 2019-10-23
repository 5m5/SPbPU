program lab_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, F2_file

   type(student), pointer  :: Group_List => Null(), Second_List => Null(), Output_List => Null()

   input_file  = "../data/person_list.txt"
   F2_file     = "F2.txt"
   output_file = "output.txt"
   
   Group_List => Read_person_list(input_file)

   if (Associated(Group_List)) then
      call Get_list(Group_List, Second_List)
      call Output_person_list(F2_file, Second_List, "P2:", "rewind")
   end if

   Output_List => Read_person_list(F2_file)

    if (Associated(Output_List)) then
      call Get_list(Output_List, Second_List)
      call Output_person_list(output_file, Second_List, "Output:", "rewind")
   end if


end program lab_3
