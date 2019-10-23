module BinaryTreeModule
   implicit none
   type node
      integer value
      type(node), pointer :: left
      type(node), pointer :: right
   end type node
   type(node), pointer :: root => null()

contains
   pure recursive subroutine addNode( value, p )
      integer, intent(in) :: value
      type(node), pointer, intent(inout) :: p
      if ( .not. associated( p ) ) then
         allocate( p )
         p%value = value
         nullify( p%left, p%right )
      else if ( value < p%value ) then
         call addNode( value, p%left )
      else
         call addNode( value, p%right )
      end if
   end subroutine addNode

   recursive logical function findValue( value, p ) result( found )
      integer, intent(in) :: value
      type(node), pointer, intent(inout) :: p
         if ( .not. associated( p ) ) then
            found = .false.
         else if ( value < p%value ) then
            found = findValue( value, p%left )
         else if ( value > p%value ) then           
            found = findValue( value, p%right )
         else
            found = .true.
         end if
   end function findValue

   recursive subroutine writeTree(fmt, p )
      character(len=*), intent(in)    :: fmt
      type(node), pointer, intent(in) :: p
      character(*), parameter         :: output = "output.txt"
      integer                         :: Out = 0

      if ( .not. associated( p ) ) return
      call writeTree( fmt, p%left )              ! Recursion to left branch

      open (file=output, newunit=Out, position="append")
         write( Out, fmt, advance = "no" ) p%value    ! Nodal value
      close (Out)

      call writeTree( fmt, p%right )             ! Recursion to right branch
   end subroutine writeTree

   recursive integer function countValue( p, x ) result( number )
      type(node), pointer, intent(in) :: p
      integer, intent(in) :: x
      number = 0
      if ( .not. associated( p ) ) return
      if ( p%value == x ) number = 1
      number = number + countValue( p%left, x ) + countValue( p%right, x )
   end function countValue

   recursive integer function countNodes( p ) result( number )
      type(node), pointer, intent(in) :: p
      number = 0
      if ( .not. associated( p ) ) return
      number = 1 + countNodes( p%left ) + countNodes( p%right )
   end function countNodes

   recursive integer function height( p ) result( h )
      type(node), pointer, intent(in) :: p
      h = 0
      if ( .not. associated( p ) ) return
      h = 1 + max( height( p%left ), height( p%right ) )
   end function height

end module BinaryTreeModule
