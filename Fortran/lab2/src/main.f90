program lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, output_file
   integer                   :: Out = 0

   type(SourceLine), pointer :: String  => Null()
   type(SourceLine), pointer :: Characters   => Null()

   F1 = "../data/string.txt"
   F2 = "../data/characters.txt"
   output_file = "output.txt"
   
   String => Read_Source(F1)
   Characters  => Read_Source(F2)

   if (Associated(String) .and. Associated(Characters)) then
      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, *) String_Contains_Imp(String, Characters)
      close (Out)
   end if

end program lab_2
