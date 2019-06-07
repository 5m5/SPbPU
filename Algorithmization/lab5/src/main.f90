program lab5
   use BinaryTreeModule
   use Environment
   implicit none
   
   character(len=*), parameter :: afmt = "( a )", ifmt = "( *( i0, 1x ) )", fmt = "( i0, 1x )"
   character(*), parameter     :: input = "../data/input.txt", output = "output.txt"
   integer                     :: N = 0, i, In = 0, Out = 0
   integer, allocatable :: A(:)

   open (file=input, newunit=In) ! С encoding=E_ может не работать на некоторых компиляторах.
      read (In, *) N
      allocate (A(N))
      read (In, *) A
   close (In)

   !A = [ 4, 10, 4, 3, 7, 5, 5, 4 ]
   open (file=output, newunit=Out)
      write( Out, afmt, advance="no" ) "Original array: " 
      write( Out, ifmt ) A
   close (Out)
   do i = 1, size( A )
      call addNode( A( i ), root )
   end do

   call writeTree( fmt, root )
   open (file=output, newunit=Out, position="append")
      write( Out, * )
      write( Out, * ) "Size = ", countNodes( root )
      write( Out, * ) "Height = ", height( root )
   close (Out)
end program lab5