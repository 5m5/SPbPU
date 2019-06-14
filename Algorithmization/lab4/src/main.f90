program lab_4
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable   :: input_polynomial_file, input_second_polynomial_file, output_file
   type(number), pointer       :: Polynomial_List => Null(), Second_Polynomial_List => Null(), Output_List => Null()
   integer                     :: Pn = 0, Qm = 0
   real(R_)                    :: Res = 0

   input_polynomial_file        = "../data/polynomial1.txt"
   input_second_polynomial_file = "../data/polynomial2.txt"
   output_file                  = "output.txt"

   Polynomial_List => Read_polynomial_list(input_polynomial_file)
   Second_Polynomial_List => Read_polynomial_list(input_second_polynomial_file)

   if (Associated(Polynomial_List) .and. Associated(Second_Polynomial_List)) then
      call Get_list(Polynomial_List, Output_List)
      call Output_polynomial_list(output_file, Output_List, "Output:", "rewind")
      call Get_list(Second_Polynomial_List, Output_List)
      call Output_polynomial_list(output_file, Output_List, "Output:", "append")
      call Calculate(Polynomial_List, Pn)
      call Calculate(Second_Polynomial_List, Qm)
      Res = Pn/Qm
      call writeResult(Res, output_file, "append")
   end if

end program lab_4
