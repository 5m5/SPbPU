program exercise_8_2
   use Environment
   use Matrix_IO
   use Matrix_process
   use Vector_IO
   
   implicit none
   character(*), parameter :: input_matrix = "../data/matrix.txt", &
   input_vector = "../data/vector.txt", output_file = "output.txt"

   real(R_), allocatable   :: B(:, :), C(:), result(:)

   B = ReadMatrix(input_matrix)

   C = ReadVector(input_vector)

   allocate(result(Size(B(:, 1))))

   call OutputMatrix(output_file, B)
   call OutputVector(output_file, C)

   call Prod(B, C, result)
   call OutputVector(output_file, result)

end program exercise_8_2
