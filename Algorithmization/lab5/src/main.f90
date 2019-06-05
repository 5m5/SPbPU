program lab5
   use BinaryTreeModule
   use Environment
   implicit none
   character(len=*), parameter :: afmt = "( a )", ifmt = "( *( i0, 1x ) )", fmt = "( i0, 1x )"
   integer, allocatable :: A(:)
   integer i
   A = [ 4, 10, 4, 3, 7, 5, 5, 4 ]
   write( *, afmt, advance="no" ) "Original array: " 
   write( *, ifmt ) A
   do i = 1, size( A )
      call addNode( A( i ), root )
   end do

   write( *, afmt, advance="no" ) "Tree-sorted array: " 
   call writeTree( fmt, root )
   write( *, * )
   write( *, * ) "Size = ", countNodes( root )
   write( *, * ) "Height = ", height( root )
end program lab5